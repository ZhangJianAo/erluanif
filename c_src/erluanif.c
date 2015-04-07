#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <string.h>
#include "erl_nif.h"

static ErlNifResourceType* erluanif_RESOURCE = NULL;

typedef struct
{
	lua_State *L;
} erluanif_handle;

static ERL_NIF_TERM erluanif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erluanif_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erluanif_dostring(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static int e_apply_continue(lua_State *L) {
	printf("apply continue called\n");
	return 1;
}

static int e_apply(lua_State *L) {
	const char *module, *func;
	ErlNifPid selfpid;

	ErlNifEnv *env = lua_touserdata(L, 1);
	if (NULL == env) {
		luaL_error(L, "erlang.apply first arg must be ErlNifEnv");
	}
	
	module = luaL_checkstring(L, 2);
	func = luaL_checkstring(L, 3);
	printf("apply get called: %s, %s\n", module, func);

	ErlNifEnv* msg_env = enif_alloc_env();
	enif_send(env, enif_self(env, &selfpid), msg_env,
		  enif_make_tuple4(msg_env,
				   enif_make_atom(msg_env, "erluanif_apply"),
				   enif_make_atom(msg_env, module),
				   enif_make_atom(msg_env, func),
				   enif_make_atom(msg_env, "haha")
			  ));
	enif_free_env(msg_env);
	
	return lua_yieldk(L, 0, 0, e_apply_continue);
}

static const struct luaL_Reg liberlang[] = {
    {"apply", e_apply},
    {NULL, NULL}
};

static ERL_NIF_TERM erluanif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	erluanif_handle* handle = enif_alloc_resource(erluanif_RESOURCE,
						      sizeof(erluanif_handle));
	handle->L = luaL_newstate();
	luaL_openlibs(handle->L);
	
	lua_newtable(handle->L);
	luaL_setfuncs (handle->L, liberlang, 0);
	lua_setglobal(handle->L, "erlang");
	
	ERL_NIF_TERM result = enif_make_resource(env, handle);
	enif_release_resource(handle);
	return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

static ERL_NIF_TERM erluanif_delete(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	erluanif_handle* handle;
	int ret = enif_get_resource(env, argv[0], erluanif_RESOURCE, (void**)&handle);
	if (!ret) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "unknow handle", ERL_NIF_LATIN1));
	}
	
	free(handle->L);
	handle->L = NULL;
	return enif_make_atom(env, "ok");
}

static char* term_to_string(ErlNifEnv* env, ERL_NIF_TERM term)
{
	unsigned strlen = 0;
	char *buff;

	if (enif_is_binary(env, term)) {
		ErlNifBinary bin;
		if (!enif_inspect_binary(env, term, &bin)) { return NULL; }
		buff = malloc(bin.size + 1);
		memcpy((void*)buff, (const void*)(bin.data), (unsigned long)(bin.size));
		buff[bin.size] = '\0';
	} else {
		if (!enif_get_list_length(env, term, &strlen)) { return NULL; }
		buff = malloc(strlen+1);
		if (!enif_get_string(env, term, buff, strlen+1, ERL_NIF_LATIN1)) { goto buff_error; }
		if ('\0' != buff[strlen]) { goto buff_error; }
	}
	
	return buff;
	
buff_error:
	free(buff);
	return NULL;
}

static ERL_NIF_TERM erluanif_dostring(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	erluanif_handle* handle;
	int ret = enif_get_resource(env, argv[0], erluanif_RESOURCE, (void**)&handle);
	if (!ret) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, "unknow handle", ERL_NIF_LATIN1));
	}

	unsigned strlen = 0;
	char *buff;
	if (enif_is_binary(env, argv[1])) {
		ErlNifBinary bin;
		if (!enif_inspect_binary(env, argv[1], &bin)) { goto error; }
		buff = malloc(bin.size + 1);
		memcpy((void*)buff, (const void*)(bin.data), (unsigned long)(bin.size));
		buff[bin.size] = '\0';
	} else {
		if (!enif_get_list_length(env, argv[1], &strlen)) { goto error; }
		buff = malloc(strlen+1);
		if (!enif_get_string(env, argv[1], buff, strlen+1, ERL_NIF_LATIN1)) { goto buff_error; }
		if ('\0' != buff[strlen]) { goto buff_error; }
	}
	
	handle->L = lua_newthread(handle->L);
	lua_getglobal(handle->L, "erlang");
	lua_pushlightuserdata(handle->L, env);
	lua_setfield(handle->L, -2, "nifenv");
	lua_pop(handle->L, 1);
	
	printf("lua code:%d %s\n", strlen, buff);
	if (LUA_OK != luaL_loadstring(handle->L, buff)) {
		free(buff);
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, lua_tostring(handle->L, -1), ERL_NIF_LATIN1));
	}
	int pcallret = lua_resume(handle->L, NULL, 0);//lua_pcallk(handle->L, 0, LUA_MULTRET, 0, 0, pcallk_continue);
	printf("pcallk ret:%d\n", pcallret);
	free(buff);
	if (LUA_OK == pcallret) {
		return enif_make_atom(env, "ok");
	}
	else if (LUA_YIELD == pcallret) {
		return enif_make_atom(env, "yield");
	}
	else {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, lua_tostring(handle->L, -1), ERL_NIF_LATIN1));
	}

buff_error:
	free(buff);
error:
	return enif_make_atom(env, "error");
}

static ERL_NIF_TERM erluanif_respond(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	erluanif_handle* handle;
	int ret = enif_get_resource(env, argv[0], erluanif_RESOURCE, (void**)&handle);
	if (!ret) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, "unknow handle", ERL_NIF_LATIN1));
	}

	char *str = term_to_string(env, argv[1]);
	lua_pushstring(handle->L, str);
	int luaret = lua_resume(handle->L, NULL, 0);
	free(str);
	if (LUA_OK == luaret) {
		return enif_make_atom(env, "ok");
	}
	else if (LUA_YIELD == luaret) {
		return enif_make_atom(env, "yield");
	}
	else {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, lua_tostring(handle->L, -1), ERL_NIF_LATIN1));
	}
}

static void erluanif_resource_cleanup(ErlNifEnv* env, void* arg)
{
	/* Delete any dynamically allocated memory stored in erluanif_handle */
	/* erluanif_handle* handle = (erluanif_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
	ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
	ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
							 "erluanif_resource",
							 &erluanif_resource_cleanup,
							 flags, NULL);
	if (rt == NULL)
		return -1;

	erluanif_RESOURCE = rt;

	return 0;
}

static ErlNifFunc nif_funcs[] =
{
	{"new", 0, erluanif_new},
	{"delete", 1, erluanif_delete},
	{"dostring", 2, erluanif_dostring},
	{"respond", 2, erluanif_respond}
};

ERL_NIF_INIT(erluanif, nif_funcs, &on_load, NULL, NULL, NULL);
