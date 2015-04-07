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

xdostring(Ref, Str) ->
    case dostring(Ref, Str) of
	yield ->
	    receive
		{erluanif_apply, M, F, A} ->
		    io:format(user, "erluanif call erlang: ~p ~p ~p~n", [M, F, A]),
		    respond(Ref, "hello from erlang"),
		    ok
	    end;
	Other ->
	    Other
    end.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertMatch({error, _}, dostring(Ref, "package.path >< package.path..';abc/?.lua'")),
    ?assertEqual(ok, dostring(Ref, "print('hello world lua!')")),
    ?assertMatch({error, _}, dostring(Ref, <<"require('main');">>)),
    ?assertEqual(ok, xdostring(Ref, <<"print(erlang.apply(erlang.nifenv, 'io', 'format', {'abcde'}))">>)),
    ?assertEqual(ok, delete(Ref)).

-endif.
