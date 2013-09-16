-module(rafter_backend_ets).

-behaviour(rafter_backend).

%% rafter_backend callbacks
-export([init/0, read/1, write/1]).

init() ->
    ets:new(capn, [set, named_table, public]),
    ok.

read({get, Table, Key}) ->
    ets:lookup(Table, Key).

write({new, Name}) ->
    ets:new((Name), [ordered_set, named_table, public]),
    {ok, Name};

write({put, Table, Key, Value}) ->
    ets:insert(Table, {Key, Value}),
    {ok, Value};

write({transaction, TableKeyPairs, Fun}) ->
    Data = [{Table, Key, ets:lookup(Table, Key)} || {Table, Key} <- TableKeyPairs],
    {Result, Updates} = Fun(Data),
    write_updates(Updates),
    {ok, Result};

write({delete, Table, Key}) ->
    {ok, ets:delete(Table, Key)}.

write_updates(Updates) ->
    [ets:insert(Table, Obj) || {Table, Obj} <- Updates].
