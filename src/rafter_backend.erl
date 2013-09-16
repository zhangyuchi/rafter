-module(rafter_backend).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 0}, {read, 1}, {write, 1}];
behaviour_info(_) ->
    undefined.

