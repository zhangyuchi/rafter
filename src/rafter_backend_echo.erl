-module(rafter_backend_echo).

-behaviour(rafter_backend).

%% Rafter backend callbacks
-export([init/0, read/1, write/1]).

init() ->
    ok.

read(Command) ->
    {ok, Command}.

write(Command) ->
    {ok, Command}.
