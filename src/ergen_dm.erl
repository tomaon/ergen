-module(ergen_dm).

-export([start/0, stop/0, run/0, cancel/0]).
-export([do_txn/0, do_cleanup_txn/0]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec run() -> boolean().
run() ->
    cast({do_txn,60000}).

-spec cancel() -> boolean().
cancel() ->
    cast({do_txn,0}).

-spec do_txn() -> boolean().
do_txn() ->
    call(do_txn).

-spec do_cleanup_txn() -> boolean().
do_cleanup_txn() ->
    call(do_cleanup_txn).

%% == private ==

-spec call(any()) -> boolean().
call(Term) ->
    case ergen_sup:call(dm, Term) of
        Value when is_boolean(Value) -> % do_cleanup_txn
            Value;
        ok ->
            true;
        _ ->
            false
    end.

-spec cast(any()) -> boolean().
cast(Term) ->
    case ergen_sup:cast(dm, Term) of
        ok ->
            true;
        _ ->
            false
    end.
