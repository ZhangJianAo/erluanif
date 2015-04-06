#include "erl_nif.h"

static ErlNifResourceType* erluanif_RESOURCE = NULL;

typedef struct
{
} erluanif_handle;

// Prototypes
static ERL_NIF_TERM erluanif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erluanif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, erluanif_new},
    {"myfunction", 1, erluanif_myfunction}
};

static ERL_NIF_TERM erluanif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    erluanif_handle* handle = enif_alloc_resource(erluanif_RESOURCE,
                                                    sizeof(erluanif_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM erluanif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
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

ERL_NIF_INIT(erluanif, nif_funcs, &on_load, NULL, NULL, NULL);
