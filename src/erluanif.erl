-module(erluanif).

-export([new/0,
	 delete/1,
	 dostring/2]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new() ->
    ?nif_stub.

delete(_Ref) ->
    ?nif_stub.

dostring(_Ref, _Str) ->
    ?nif_stub.

respond(_Ref, _Res) ->
    ?nif_stub.

call(_Res, _FunName, _Args) ->
    ?nif_stub.

callfunc(Res, FunName, Args) ->
    lua_return(call(Res, FunName, Args)).

eval(Ref, Str) ->
    lua_return(dostring(Ref, Str)).

lua_return({yield, Ref}) ->
    receive
	{erluanif_apply, M, F, A} ->
	    Ret = apply(M, F, A),
	    io:format(user, "erluanif call erlang: ~p ~p ~p ~p~n", [M, F, A, Ret]),
	    lua_return(respond(Ref, Ret))
    end;
lua_return(Other) ->    
    Other.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertMatch({error, _}, dostring(Ref, "package.path >< package.path..';abc/?.lua'")),
    ?assertEqual(ok, dostring(Ref, "print('hello world lua!')")),
    ?assertMatch({error, _}, dostring(Ref, <<"require('main');">>)),
    ?assertEqual(ok, eval(Ref, <<"print(erlang.apply(erlang.nifenv, 'io', 'format', {'abcde'}))">>)),
    ?assertEqual(ok, dostring(Ref, <<"function test(msg) print(msg); print(erlang.apply(erlang.nifenv, 'io', 'format', {'abcde'})) end">>)),
    ?assertEqual(ok, callfunc(Ref, <<"test">>, [<<"heihei">>])),
    ?assertEqual(ok, delete(Ref)).

-endif.
