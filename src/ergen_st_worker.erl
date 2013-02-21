-module(ergen_st_worker).

-export([start_link/2, stop/1]).
-export([cast/2, cast/3]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("ergen.hrl").

-define(SEC(N), (N * 1000 * 1000)).

%% == public ==

-spec start_link([any()],pos_integer()) -> {ok,pid()}|{error,_}.
start_link(Args, Id)
  when is_list(Args), is_integer(Id), Id > 0 ->
    case gen_server:start_link(?MODULE, [Id], []) of
        {ok, Pid} ->
            case gen_server:call(Pid, {setup,Args}) of
                ok ->
                    {ok, Pid};
                {error, Reason} ->
                    stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:call(Pid, stop).

cast(Pid, {cast,{?CMD_BH_DO_TXN_BV,_,N}}=T) -> cast(Pid, T, N >= ?SEC(3));
cast(Pid, {cast,{?CMD_BH_DO_TXN_CP,_,N}}=T) -> cast(Pid, T, N >= ?SEC(3));
cast(Pid, {cast,{?CMD_BH_DO_TXN_MF,_,N}}=T) -> cast(Pid, T, N >= ?SEC(2));
cast(Pid, {cast,{?CMD_BH_DO_TXN_MW,_,N}}=T) -> cast(Pid, T, N >= ?SEC(3));
cast(Pid, {cast,{?CMD_BH_DO_TXN_SD,_,N}}=T) -> cast(Pid, T, N >= ?SEC(3));
cast(Pid, {cast,{?CMD_BH_DO_TXN_TL,_,N}}=T) -> cast(Pid, T, N >= ?SEC(3));
cast(Pid, {cast,{?CMD_BH_DO_TXN_TO,_,N}}=T) -> cast(Pid, T, N >= ?SEC(2));
cast(Pid, {cast,{?CMD_BH_DO_TXN_TR,_,N}}=T) -> cast(Pid, T, N >= ?SEC(2));
cast(Pid, {cast,{?CMD_BH_DO_TXN_TS,_,N}}=T) -> cast(Pid, T, N >= ?SEC(1));
cast(Pid, {cast,{?CMD_BH_DO_TXN_TU,_,N}}=T) -> cast(Pid, T, N >= ?SEC(3));
cast(Pid, Term) -> cast(Pid, Term, false).

cast(Pid, Term, Delay)
  when is_pid(Pid), is_boolean(Delay) ->
    gen_server:cast(Pid, erlang:append_element(Term,Delay)).

%% == behaviour: gen_server ==

-record(state, {
          id :: pos_integer(),
          channel :: pid(),
          server :: pid(),
          tref :: timer:tref()
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({setup,Args}, _From, #state{}=S) ->
    try lists:foldl(fun setup/2, S, Args) of
        State ->
            {reply, ok, State}
    catch
        {Reason, State} ->
            {reply, {error,Reason}, State}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast({stat,Interval,false}, #state{tref=T}=S)
  when Interval =< 0, undefined =/= T ->
    {ok, cancel} = timer:cancel(T),
    io:format("~p~n", [folsom_statistics()]),
    {noreply, S#state{tref = undefined}};
handle_cast({stat,Interval,false}, #state{}=S)
  when Interval > 0 ->
    {ok, TRef} = timer:send_after(Interval, {show,Interval,0}),
    {noreply, S#state{tref = TRef}};
handle_cast({cast,{C,V,_},D}, #state{}=S) ->
    if C =:= ?CMD_BH_DO_TXN_TR ->
            folsom_incr(C);
       true ->
            ok
    end,
    folsom_incr({C,V,D}),
    {noreply, S};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({show,Interval,N}, #state{}=S) when N =< 0 ->
    L= [
        {"|",1},
        {"DM",3},
        {"|",2},
        {"BV",6},{"CP",6},{"MW",6},{"SD",6},{"TL",6},{"TO",6},{"TS",6},{"TU",6},
        {"|",2},
        {"MF",5},{"TR",6},
        {"|",2},
        {"tpsE",6},
        {"|",2}
       ],
    L1 = lists:flatten([ string:right(K,V) || {K,V} <- L ]),
    L2 = [ case C of $| -> $+; _ -> $- end || C <- L1],
    io:format("~n~s~n~s~n", [L1,L2]),
    handle_info({show,Interval,20}, S);
handle_info({show,Interval,N}, #state{}=S) ->
    L = [
         {{?CMD_BH_DO_TXN_DM,true,false},"~5b"},
         {{?CMD_BH_DO_TXN_BV,true,false},"~7b"},
         {{?CMD_BH_DO_TXN_CP,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_MW,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_SD,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_TL,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_TO,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_TS,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_TU,true,false},"~6b"},
         {{?CMD_BH_DO_TXN_MF,true,false},"~7b"},
         {{?CMD_BH_DO_TXN_TR,true,false},"~6b"},
         {?CMD_BH_DO_TXN_TR,"~8.1f"}
        ],
    io:format("~s~n", [[ io_lib:format(F,[folsom_reset(T,Interval)]) || {T,F} <- L ]]),
    {ok, TRef} = timer:send_after(Interval, {show,Interval,N-1}),
    {noreply, S#state{tref = TRef}};
handle_info({#'basic.return'{reply_code=C,reply_text=T},_}, #state{}=S) ->
    {stop, {error,{C,T}}, S}; % TODO
handle_info({'DOWN',_MRef,process,P,Info}, #state{channel=P}=S) ->
    {stop, Info, S#state{channel = undefined}};
handle_info({'EXIT',P,Reason}, #state{server=P}=S) ->
    {stop, Reason, S#state{server = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

-spec cleanup(state()) -> ok.
cleanup(#state{server=P}=S)
  when is_pid(P) ->
    _ = ergen_rpc_server:stop(P),
    cleanup(S#state{server = undefined});
cleanup(#state{channel=P}=S)
  when is_pid(P) ->
    _ = ergen_amqp:close(P),
    cleanup(S#state{channel = undefined});
cleanup(#state{}) ->
    %%io:format("~p [~p:cleanup]~n", [self(),?MODULE]),
    ergen_util:flush().

-spec setup([any()]) -> {ok,state()}|{stop,_}.
setup([Id]) ->
    %%io:format("~p [~p:setup] id=~w~n", [self(),?MODULE,Id]),
    process_flag(trap_exit, true),
    folsom_init(),
    {ok, #state{id = Id}};
setup(_) ->
    {stop, badarg}.

-spec setup({atom(),any()},state()) -> state().
setup({connection,Connection}, #state{channel=undefined}=S)
  when is_pid(Connection) ->
    case ergen_amqp:open(Connection, {amqp_selective_consumer,[]}) of
        {ok, Pid} ->
            monitor(process, Pid),
            amqp_channel:register_return_handler(Pid, self()),
            amqp_selective_consumer:register_default_consumer(Pid, self()),
            S#state{channel = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({subscribe,Args}, #state{channel=C,server=undefined}=S)
  when is_list(Args) ->
    case ergen_rpc_server:start_link(C) of
        {ok, Pid} ->
            lists:foldl(fun setup_server/2, S#state{server = Pid}, Args);
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({K,_}, #state{}=S) ->
    throw({{badmatch,K},S}).

-spec setup_server(binary(),state()) -> state().
setup_server(Queue, #state{server=P}=S)
  when is_binary(Queue), undefined =/= P ->
    Self = self(),
    Fun = fun(Term) -> cast(Self, {cast,Term}) end,
    case ergen_rpc_server:subscribe(P, Queue, Fun) of
        ok ->
            S;
        {error, Reason} ->
            throw({Reason,S})
    end;
setup_server(_Args, #state{}=S) ->
    throw({badarg,S}).

%% == private ==

folsom_init() ->
    folsom_metrics:new_histogram(?MODULE, uniform, infinity, 0.015). % TODO

folsom_statistics() ->
    folsom_metrics:get_histogram_statistics(?MODULE).

folsom_incr(Name) ->
    case folsom_metrics:metric_exists(Name) of
        false ->
            folsom_metrics:new_counter(Name);
        true ->
            ok
    end,
    folsom_metrics:notify(Name, {inc,1}).

folsom_reset(Name) ->
    case folsom_metrics:get_metric_value(Name) of
        {error,_,_} ->
            0;
        Value ->
            folsom_metrics:notify(Name, {dec,Value}),
            Value
    end.

folsom_reset(?CMD_BH_DO_TXN_TR=N, Interval) ->
    Value = folsom_reset(N) / (Interval / 1000),
    folsom_metrics:notify({?MODULE,Value}),
    Value;
folsom_reset(Name, _Interval) ->
    folsom_reset(Name).
