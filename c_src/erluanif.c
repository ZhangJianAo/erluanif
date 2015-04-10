#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <string.h>
#include <assert.h>
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
	return 1;
}

static ERL_NIF_TERM lua_to_term(ErlNifEnv* env, lua_State* L, int pos)
{
	int luat = lua_type(L, pos);
	if (LUA_TNIL == luat) {
		return enif_make_atom(env, "nil");
	}
	else if (LUA_TBOOLEAN == luat) {
		if (lua_toboolean(L, pos)) {
			return enif_make_atom(env, "true");
		} else {
			return enif_make_atom(env, "false");
		}
	}
	else if (LUA_TNUMBER == luat) {
		double n = lua_tonumber(L, pos);
		if (n == (int)n) {
			return enif_make_int(env, n);
		} else {
			return enif_make_double(env, n);
		}
	}
	else if (LUA_TSTRING == luat) {
		size_t len;
		const char* str = lua_tolstring(L, pos, &len);
		ERL_NIF_TERM atom;
		if (enif_make_existing_atom(env, str, &atom, ERL_NIF_LATIN1)) {
			return atom;
		} else {
			ErlNifBinary bin;
			enif_alloc_binary(len, &bin);
			memcpy(bin.data, str, len);
			return enif_make_binary(env, &bin);
		}
	}
	else if (LUA_TTABLE == luat) {
		ERL_NIF_TERM list = enif_make_list(env, 0);
		lua_pushnil(L);
		while (lua_next(L, pos) != 0) {
			ERL_NIF_TERM item;
			if (lua_type(L, -2) == LUA_TSTRING) {
				int luatop = lua_gettop(L);
				item = enif_make_tuple2(env, lua_to_term(env, L, luatop-1), lua_to_term(env, L, luatop));
			} else {
				item = lua_to_term(env, L, lua_gettop(L));
			}
			list = enif_make_list_cell(env, item, list);
			lua_pop(L, 1);
		}
		enif_make_reverse_list(env, list, &list);
		return list;
	}
	else {
		luaL_error(L, "can't convert lua type %d to erlang", luat);
		return enif_make_atom(env, "error");
	}
}

static int e_apply(lua_State *L) {
	const char *module, *func;
	ErlNifPid selfpid;

	ErlNifEnv *env = lua_touserdata(L, 1);
	if (NULL == env) {
		luaL_error(L, "erlang.apply first arg must be erlang.nifenv");
	}
	
	module = luaL_checkstring(L, 2);
	func = luaL_checkstring(L, 3);

	ErlNifEnv* msg_env = enif_alloc_env();
	enif_send(env, enif_self(env, &selfpid), msg_env,
		  enif_make_tuple4(msg_env,
				   enif_make_atom(msg_env, "erluanif_apply"),
				   enif_make_atom(msg_env, module),
				   enif_make_atom(msg_env, func),
				   lua_to_term(msg_env, L, 4)
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

static void term_to_lua(ErlNifEnv* env, ERL_NIF_TERM term, lua_State* L)
{
	char *buff;
	unsigned len;
	if (enif_is_atom(env, term)) {
		if (!enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1)) { return; }
		len++; // for terminating null character
		buff = malloc(len);
		enif_get_atom(env, term, buff, len, ERL_NIF_LATIN1);
		if (strcmp("null", buff) == 0) {
			lua_pushnil(L);
		} else {
			lua_pushstring(L, buff);
		}
		free(buff);
	}
	else if (enif_is_binary(env, term)) {
		ErlNifBinary bin;
		if (!enif_inspect_binary(env, term, &bin)) { return; }
		lua_pushlstring(L, (char*)bin.data, bin.size);
	}
	else if (enif_is_number(env, term)) {
		int i;
		double d;
		if (enif_get_int(env, term, &i)) {
			lua_pushinteger(L, i);
		}
		else if (enif_get_double(env, term, &d)) {
			lua_pushnumber(L, d);
		}
		else {
			printf("can't convert erlang number to lua");
		}
	}
	else {
		printf("can't convert erlang term to lua");
	}
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
	
	if (LUA_OK != luaL_loadstring(handle->L, buff)) {
		free(buff);
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, lua_tostring(handle->L, -1), ERL_NIF_LATIN1));
	}
	int pcallret = lua_resume(handle->L, NULL, 0);
	free(buff);
	if (LUA_OK == pcallret) {
		return enif_make_atom(env, "ok");
	}
	else if (LUA_YIELD == pcallret) {
		return enif_make_tuple2(env,
					enif_make_atom(env, "yield"),
					enif_make_resource(env, handle));
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

static int lua_push_function(lua_State* L, char* funcname)
{
	int luatop = lua_gettop(L);
	
	lua_getglobal(L, funcname);
	if (lua_isfunction(L, -1)) { return 1; }

	funcname = strdup(funcname);
	
	char* dot = strchr(funcname, '.');
	if (NULL != dot) {
		*dot = '\0';
		lua_getglobal(L, funcname);
		if (lua_istable(L, -1)) {
			while (NULL != dot) {
				char* field = dot + 1;
				dot = strchr(field, '.');
				if (NULL != dot) { *dot = '\0'; }
				lua_getfield(L, -1, field);
				if (! lua_istable(L, -1)) { break; }
			}
		}
	}

	if (lua_isfunction(L, -1)) {
		return 1;
	} else {
		lua_pop(L, lua_gettop(L) - luatop);
		assert(luatop == lua_gettop(L));
		return 0;
	}
}

static ERL_NIF_TERM luaret_to_term(ErlNifEnv* env, lua_State* L)
{
	int luatop = lua_gettop(L);
	if (luatop > 0) {
		return enif_make_tuple2(env,
					enif_make_atom(env, "ok"),
					lua_to_term(env, L, luatop)
			);
	}
	else {
		return enif_make_atom(env, "ok");
	}

}

static ERL_NIF_TERM erluanif_call(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	if (argc != 3) {
		return enif_make_badarg(env);
	}
	
	erluanif_handle* handle;
	int ret = enif_get_resource(env, argv[0], erluanif_RESOURCE, (void**)&handle);
	if (!ret) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, "unknow handle", ERL_NIF_LATIN1));
	}

	char* funcname = term_to_string(env, argv[1]);
	
	handle->L = lua_newthread(handle->L);
	lua_getglobal(handle->L, "erlang");
	lua_pushlightuserdata(handle->L, env);
	lua_setfield(handle->L, -2, "nifenv");
	lua_pop(handle->L, 1);

	if (!lua_push_function(handle->L, funcname)) {
		free(funcname);
		char error_info[256];
		int printlen = snprintf(error_info, 256, "unknow function:%s", funcname);
		if (printlen < 0 || printlen >= 256) {
			sprintf(error_info, "unknow function name");
		}
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, error_info, ERL_NIF_LATIN1));
	}

	unsigned func_argc;
	enif_get_list_length(env, argv[2], &func_argc);

	ERL_NIF_TERM item;
	ERL_NIF_TERM func_argv = argv[2];
	while (enif_get_list_cell(env, func_argv, &item, &func_argv)) {
		term_to_lua(env, item, handle->L);
	}

	int pcallret = lua_resume(handle->L, NULL, func_argc);
	free(funcname);
	if (LUA_OK == pcallret) {
		return luaret_to_term(env, handle->L);
	}
	else if (LUA_YIELD == pcallret) {
		return enif_make_tuple2(env,
					enif_make_atom(env, "yield"),
					enif_make_resource(env, handle));
	}
	else {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, lua_tostring(handle->L, -1), ERL_NIF_LATIN1));
	}
}

static ERL_NIF_TERM erluanif_respond(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
	erluanif_handle* handle;
	int ret = enif_get_resource(env, argv[0], erluanif_RESOURCE, (void**)&handle);
	if (!ret) {
		return enif_make_tuple2(env, enif_make_atom(env, "error"),
					enif_make_string(env, "unknow handle", ERL_NIF_LATIN1));
	}

	term_to_lua(env, argv[1], handle->L);
	int luaret = lua_resume(handle->L, NULL, 0);
	if (LUA_OK == luaret) {
		return luaret_to_term(env, handle->L);
	}
	else if (LUA_YIELD == luaret) {
		return enif_make_tuple2(env,
					enif_make_atom(env, "yield"),
					enif_make_resource(env, handle));
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
	{"respond", 2, erluanif_respond},
	{"call", 3, erluanif_call}
};

ERL_NIF_INIT(erluanif, nif_funcs, &on_load, NULL, NULL, NULL);
