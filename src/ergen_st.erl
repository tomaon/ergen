-module(ergen_st).

-export([start/0, stop/0, run/0, run/1, cancel/0]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec run() -> boolean().
run() ->
    run(5000).

-spec run(non_neg_integer()) -> boolean().
run(Interval) ->
    cast({stat,Interval}).

-spec cancel() -> boolean().
cancel() ->
    cast({stat,0}).

%% == private ==

-spec cast(any()) -> boolean().
cast(Term) ->
    case ergen_sup:cast(st, Term) of
        ok ->
            true;
        _ ->
            false
    end.
