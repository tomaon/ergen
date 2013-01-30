-module(ergen_ce).

-export([start/0, stop/0, run/0, run/1, cancel/0]).
-export([do_txn/0]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec run() -> boolean().
run() ->
    run(300).

-spec run(non_neg_integer()) -> boolean().
run(Interval) ->
    cast({do_txn,Interval}).

-spec cancel() -> boolean().
cancel() ->
    cast({do_txn,0}).

-spec do_txn() -> boolean()|{error,_}.
do_txn() ->
    call(do_txn).

%% == private ==

-spec call(any()) -> any().
call(Term) ->
    ergen_sup:call(ce, Term).

-spec cast(any()) -> boolean().
cast(Term) ->
    case ergen_sup:cast(ce, Term) of
        ok ->
            true;
        _ ->
            false
    end.
