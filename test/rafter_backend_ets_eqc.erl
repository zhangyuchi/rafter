-module(rafter_backend_ets_eqc).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(eqc_statem).

%% eqc_statem exports
-export([command/1, initial_state/0, next_state/3, postcondition/3,
         precondition/2, invariant/1]).

%% functions used in property callbacks
-export([put_data/5, del_data/4]).

-record(state, {
    tables=sets:new() :: sets:set(atom()),
    data=[] :: [{{Table :: atom(), Key :: binary()}, Value :: term()}]}).

-define(QC_OUT(P),
    eqc:on_output(fun(Str, Args) ->
                io:format(user, Str, Args) end, P)).

eqc_test_() ->
    {spawn,
       [%% Run the quickcheck tests
        {timeout, 120,
         ?_assertEqual(true, 
             eqc:quickcheck(
                 ?QC_OUT(eqc:numtests(50, prop_backend()))))}
       ]
    }.

prop_backend() ->
    ?FORALL(Cmds, 
            more_commands(20, commands(?MODULE)), 
            aggregate(command_names(Cmds), 
                begin
                    rafter_backend_ets:init(),
                    {H, S, Res} = run_commands(?MODULE, Cmds),
                    eqc_statem:pretty_commands(?MODULE,
                                               Cmds,
                                               {H, S, Res},
                                               cleanup(S, Res))
                end)).

cleanup(State, Res) ->
    rafter_backend_ets:stop(),
    [ets:delete(Table) || Table <- sets:to_list(State#state.tables)],
    Res =:= ok.

%% ====================================================================
%% eqc_statem callbacks
%% ====================================================================
initial_state() ->
    #state{}.

command(#state{}) ->
    frequency([
        {10, {call, rafter_backend_ets, write, [{new, table_gen()}]}},
        {1, {call, rafter_backend_ets, write, [{delete, table_gen()}]}},
        {100, {call, rafter_backend_ets, write, [{delete, table_gen(), key_gen()}]}},
        {200, {call, rafter_backend_ets, write, 
                [{put, table_gen(), key_gen(), value_gen()}]}}]).

precondition(#state{}, _) ->
    true.

next_state(#state{tables=Tables}=S, _Result,
    {call, rafter_backend_ets, write, [{new, Table}]}) ->
        S#state{tables={call, sets, add_element, [Table, Tables]}};

next_state(#state{data=Data, tables=Tables}=S, _Result,
    {call, rafter_backend_ets, write, [{put, Table, Key, Value}]}) ->
        S#state{data={call, ?MODULE, put_data, [Table, Key, Value, Data, Tables]}};

next_state(#state{tables=Tables}=S, _,
    {call, rafter_backend_ets, write, [{delete, Table}]}) ->
        S#state{tables={call, sets, del_element, [Table, Tables]}};

next_state(#state{data=Data, tables=Tables}=S, _,
    {call, rafter_backend_ets, write, [{delete, Table, Key}]}) ->
        S#state{data={call, ?MODULE, del_data, [Table, Key, Data, Tables]}}.

postcondition(#state{}, 
    {call, rafter_backend_ets, write, [{new, Table}]},
    {ok, Table}) ->
        true;
postcondition(#state{tables=Tables}, 
    {call, rafter_backend_ets, write, [{new, Table}]},
    {error, badarg}) ->
        sets:is_element(Table, Tables);

postcondition(#state{tables=Tables},
    {call, rafter_backend_ets, write, [{put, Table, _Key, Value}]},
    {ok, Value}) ->
        sets:is_element(Table, Tables);
postcondition(#state{tables=Tables},
    {call, rafter_backend_ets, write, [{put, Table, _Key, _Value}]},
    {error, badarg}) ->
        not sets:is_element(Table, Tables);

postcondition(#state{tables=Tables},
    {call, rafter_backend_ets, write, [{delete, Table}]}, {ok, Table}) ->
        sets:is_element(Table, Tables);
postcondition(#state{tables=Tables},
    {call, rafter_backend_ets, write, [{delete, Table}]}, {error, badarg}) ->
        not sets:is_element(Table, Tables);

postcondition(#state{tables=Tables},
    {call, rafter_backend_ets, write, [{delete, Table, _Key}]}, {ok, true}) ->
        sets:is_element(Table, Tables);

postcondition(#state{tables=Tables},
    {call, rafter_backend_ets, write, [{delete, Table, _Key}]}, {error, badarg}) ->
        not sets:is_element(Table, Tables).

invariant(State) ->
    tables_exist(State) andalso
    data_is_correct(State).

%% ====================================================================
%% Invariants 
%% ====================================================================
tables_exist(#state{tables=Tables}) ->
    EtsTables = sets:from_list(ets:all()),
    sets:is_subset(Tables, EtsTables).

data_is_correct(#state{data=Data}) ->
    lists:all(fun({{Table, Key}, Value}) ->
                [{Key, Value}] =:= ets:lookup(Table, Key)
              end, Data).

%% ====================================================================
%% Internal Functions
%% ====================================================================
put_data(Table, Key, Value, Data, Tables) ->
    case sets:is_element(Table, Tables) of
        true ->
            lists:keystore({Table, Key}, 1, Data, {{Table, Key}, Value});
        false ->
            Data
    end.

del_data(Table, Key, Data, Tables) ->
    case sets:is_element(Table, Tables) of
        true ->
            lists:keydelete({Table, Key}, 1, Data);
        false ->
            Data
    end.

%% ====================================================================
%% EQC Generators
%% ====================================================================

table_gen() ->
    oneof([list_to_atom("table_"++integer_to_list(L)) || L <- lists:seq(0, 100)]).

key_gen() ->
    binary().

value_gen() ->
    int().

-endif.
