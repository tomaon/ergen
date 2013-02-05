-module(ergen).

-export([start/0, stop/0]).
-export([run/0, run/1, cancel/0]).

%% == public ==

-spec start() -> ok.
start() ->
    lists:foreach(fun application:start/1, deps()).

-spec stop() -> ok.
stop() ->
    lists:foreach(fun application:stop/1, lists:reverse(deps())).

-spec run() -> ok.
run() ->
    run(300).

-spec run(non_neg_integer()) -> ok.
run(Interval)
  when is_integer(Interval), Interval > 0 ->
    L = [
         {ergen_mee, set_base_time, []},
         {ergen_dm, do_cleanup_txn, []},
         {ergen_st, run, []},
         {ergen_dm, run, []},
         {ergen_ce, run, [Interval]}
        ],
    lists:takewhile(fun({M,F,A}) -> apply(M,F,A) end, L),
    ok.

-spec cancel() -> ok.
cancel() ->
    L = [
         {ergen_st, cancel, []},
         {ergen_ce, cancel, []},
         {ergen_dm, cancel, []}
        ],
    lists:takewhile(fun({M,F,A}) -> apply(M,F,A) end, L),
    ok.

%% == private ==

-spec deps() -> [atom()].
deps() ->
    _ = application:load(?MODULE),
    case application:get_key(?MODULE, included_applications) of
        {ok, List} ->
            lists:foldl(fun proplists:delete/2, List, []);
        _  ->
            []
    end.
