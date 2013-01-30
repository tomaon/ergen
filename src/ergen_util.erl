-module(ergen_util).

-export([choose/1]).
-export([do_while/3]).
-export([filter/3]).
-export([flush/0]).
-export([map2/2]).
-export([split/2]).
-export([suffix/1]).

-spec filter(function(),non_neg_integer(),[tuple()]) -> [pid()].
filter(Fun, N, List) ->
    lists:filter(Fun, lists:map(fun(T) -> element(N,T) end, List)).

-spec choose([pid()]) -> pid()|undefined.
choose([]) ->
    undefined;
choose(List) ->
    <<N:16>> = crypto:rand_bytes(2),
    lists:nth(1 + N rem length(List), List).

-spec do_while(function(),[any()],any()) -> any().
do_while(_Fun, [], _Args) ->
    true;
do_while(Fun, [H|T], Args) ->
    case Fun(H, Args) of
        true ->
            do_while(Fun, T, Args);
        Term ->
            Term
    end.

-spec flush() -> ok.
flush() ->
    receive _ -> flush() after 0 -> ok end.

-spec map2(function(),[any()]) -> [any()].
map2(Fun, List) ->
    map2(Fun, List, []).

-spec map2(function(),[any()],[any()]) -> [any()].
map2(_Fun, [], List) ->
    lists:reverse(List);
map2(Fun, [H|T], List) ->
    E = Fun(H, length(List)+1),
    map2(Fun, T, [E|List]).

-spec split(non_neg_integer(),[any()]) -> [[any()]].
split(N, List) ->
    split(N, List, []).

-spec split(non_neg_integer(),[any()],[[any()]]) -> [[any()]].
split(N, List, Result) when length(List) =< N ->
    lists:reverse([List|Result]);
split(N, List, Result) ->
    {H,T} = lists:split(N, List),
    split(N, T, [H|Result]).

-spec suffix(string()) -> string().
suffix(Str) ->
    lists:last(string:tokens(Str, "_")).
