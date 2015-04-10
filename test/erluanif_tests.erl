-module(erluanif_tests).
-include_lib("eunit/include/eunit.hrl").
-export([return_null/0]).

all_test_() ->
    {"erluanif tests",
     {setup,
      fun start/0,
      fun stop/1,
      [
       fun test_load/0,
       fun test_lua_ret/0
      ]}
    }.

start() ->
    {ok, Ref} = erluanif:new(),
    Ref.
    
stop(Ref) ->
    erluanif:delete(Ref).

return_null() ->
    null.

test_load() ->
    {ok, Ref} = erluanif:new(),
    TestPath = filename:join(["..", "test"]),
    erluanif:add_package_path(Ref, TestPath),
    ?assertEqual(ok, erluanif:dostring(Ref, <<"require('main')">>)),
    Msg = <<"hello world!">>,
    ?assertEqual({ok, Msg}, erluanif:callfunc(Ref, "process", [Msg])),
    ?assertEqual({ok, <<"test_table:", Msg/binary>>}, erluanif:callfunc(Ref, "test_table.process", [Msg])),
    ?assertEqual({ok, 16}, erluanif:callfunc(Ref, "call_erlang", [1])),
    ?assertEqual({ok, true}, erluanif:eval(Ref, <<"return nil == erlang.apply(erlang.nifenv, 'erluanif_tests', 'return_null', {})">>)),
    erluanif:delete(Ref).


test_lua_ret() ->
    {ok, Ref} = erluanif:new(),
    TestPath = filename:join(["..", "test"]),
    erluanif:add_package_path(Ref, TestPath),
    ?assertEqual(ok, erluanif:dostring(Ref, <<"require('main')">>)),
    {ok, LargeRet} = erluanif:callfunc(Ref, "large_ret", []),
    ?assertEqual(1012, byte_size(LargeRet)),
    erluanif:delete(Ref).

    
