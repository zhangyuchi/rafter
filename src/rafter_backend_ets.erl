-module(rafter_backend_ets).

-behaviour(rafter_backend).

%% rafter_backend callbacks
-export([init/0, stop/0, read/1, write/1]).

init() ->
    ets:new(rafter_backend_ets, [set, named_table, public]),
    ok.

stop() ->
    ets:delete(rafter_backend_ets).

read({get, Table, Key}) ->
    ets:lookup(Table, Key).

write({new, Name}) ->
    try
        ets:new((Name), [ordered_set, named_table, public]),
        {ok, Name}
    catch _:E ->
        {error, E}
    end;

write({put, Table, Key, Value}) ->
    try 
        ets:insert(Table, {Key, Value}),
        {ok, Value}
    catch _:E ->
        {error, E}
    end;

write({transaction, TableKeyPairs, Fun}) ->
    Data = [{Table, Key, ets:lookup(Table, Key)} || {Table, Key} <- TableKeyPairs],
    {Result, Updates} = Fun(Data),
    write_updates(Updates),
    {ok, Result};

write({delete, Table}) ->
    try
        {ok, ets:delete(Table)}
    catch _:E ->
        {error, E}
    end;

write({delete, Table, Key}) ->
    try
        {ok, ets:delete(Table, Key)}
    catch _:E ->
        {error, E}
    end.

write_updates(Updates) ->
    [ets:insert(Table, Obj) || {Table, Obj} <- Updates].
