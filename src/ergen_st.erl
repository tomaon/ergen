-module(ergen_st).

-export([start/0, stop/0]).

%% == public ==

-spec start() -> ok|{error,_}.
start() ->
    application:start(?MODULE).

-spec stop() -> ok|{error,_}.
stop() ->
    application:stop(?MODULE).
