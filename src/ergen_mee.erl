-module(ergen_mee).

-export([start/0, stop/0]).
-export([set_base_time/0, submit_trade_result/1, generate_trade_result/0]).
-export([disable_ticker_tape/0, enable_ticker_tape/0]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).

-spec set_base_time() -> boolean().
set_base_time() ->
    cast(set_base_time).

-spec submit_trade_result(any()) -> integer()|{error,_}.
submit_trade_result(Term) ->
    call({submit_trade_result,Term}).

-spec generate_trade_result() -> integer()|{error,_}.
generate_trade_result() ->
    call(generate_trade_result).

-spec disable_ticker_tape() -> boolean().
disable_ticker_tape() ->
    cast(disable_ticker_tape).

-spec enable_ticker_tape() -> boolean().
enable_ticker_tape() ->
    cast(enable_ticker_tape).

%% == private ==

-spec call(any()) -> any().
call(Term) ->
    ergen_sup:call(mee, Term).

-spec cast(any()) -> boolean().
cast(Term) ->
    case ergen_sup:cast(mee, Term) of
        ok ->
            true;
        _ ->
            false
    end.
