-module(ergen_app).

-behaviour(application).
-export([start/2, stop/1]).

%% == behaviour: application ==

-record(state, {
          type :: atom(),
          groups :: [[non_neg_integer()]],
          driver = [] :: [any()],
          sup :: pid()
         }).

-type(state() :: #state{}).

start(StartType, StartArgs) ->
    try lists:foldl(fun setup/2, setup(StartType), update(StartArgs)) of
        #state{sup=P}=S ->
            {ok, P, S}
    catch
        {Reason, State} ->
            cleanup(State),
            {error,Reason}
    end.

stop(#state{}=S) ->
    cleanup(S).

%% == private: state ==

-spec cleanup(state()) -> ok.
cleanup(#state{sup=P}=S)
  when is_pid(P) ->
    _ = ergen_sup:stop(P),
    cleanup(S#state{sup = undefined});
cleanup(#state{driver=[T|_]}=S) ->
    _ = ergen_driver:unload(T),
    cleanup(S#state{driver = []});
cleanup(#state{}) ->
    %%io:format("~p [~p:cleanup]~n", [self(),?MODULE]),
    ergen_util:flush().

-spec setup([any()]) -> state().
setup(_) ->
    {ok, Application} = application:get_application(),
    %%io:format("~p [~p:setup] application=~p~n", [self(),?MODULE,Application]),
    #state{type = list_to_atom(ergen_util:suffix(atom_to_list(Application)))}.

-spec setup({atom(),any()},state()) -> state().
setup({groups,{seq,From,To,Incr,N}}, #state{groups=undefined}=S)
  when is_integer(From), is_integer(To), is_integer(Incr), is_integer(N) ->
    setup({groups,ergen_util:split(N,lists:seq(From,To,Incr))}, S);
setup({groups,Groups}, #state{groups=undefined}=S)
  when is_list(Groups) ->
    S#state{groups = Groups};
setup({driver,Driver}, #state{driver=[]}=S)
  when is_list(Driver) ->
    case ergen_driver:load([{name,proplists:get_value(name,Driver)}]) of
        {ok, Tuple} ->
            Path = filename:join(os:getenv("EGEN_HOME"), "flat_in"),
            S#state{driver = [Tuple, [{path,Path}|proplists:get_value(options,Driver,[])]]};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({server,Server}, #state{type=T,groups=G,driver=D}=S)
  when is_list(Server) ->
    case ergen_sup:start_link(T, D, [Server], G) of
        {ok, Pid} ->
            S#state{sup = Pid};
        {error, Reason} ->
            throw({Reason, S})
    end;
setup({K,_}, #state{}=S) ->
    throw({{badmatch,K},S}).

%% == private: etc ==

-spec update([any()]) -> [any()].
update(List) ->
    {ok, Application} = application:get_application(),
    update(List, application:get_all_env(Application), []).

-spec update([any()],[any()],[any()]) -> [any()].
update([], _List2, List3) ->
    lists:reverse(List3);
update([{K,V}=H|T], List2, List3)  ->
    case lists:keyfind(K, 1, List2) of
        false ->
            update(T, List2, [H|List3]);
        {K, L} when is_list(L) ->
            update(T, List2, [{K,update(V, L, [])}|List3]);
        Tuple ->
            update(T, List2, [Tuple|List3])
    end;
update([H|T], List2, List3) ->
    update(T, List2, [H|List3]).
