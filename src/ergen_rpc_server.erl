-module(ergen_rpc_server).

-export([start_link/1, stop/1]).
-export([subscribe/3, cancel/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% == public ==

-spec start_link(pid()) -> {ok,pid()}|{error,_}.
start_link(Channel)
  when is_pid(Channel) ->
    gen_server:start_link(?MODULE, [Channel], []).

-spec stop(pid()) -> ok.
stop(Pid)
  when is_pid(Pid) ->
    gen_server:call(Pid, stop).

-spec subscribe(pid(),binary(),function()) -> ok|{error,_}.
subscribe(Pid, Queue, Fun)
  when is_pid(Pid), is_binary(Queue), is_function(Fun) ->
    gen_server:call(Pid, {subscribe,Queue,Fun}).

-spec cancel(pid(),binary()) -> ok|{error,_}.
cancel(Pid, Queue)
  when is_pid(Pid), is_binary(Queue) ->
    gen_server:call(Pid, {cancel,Queue}).

%% == behaviour: gen_server ==

-record(state, {
          channel :: pid(),
          assigned   = [] :: [any()], % [{ConsumerTag,Queue,Fun}]
          unassigned = [] :: [any()]  % [{ConsumerTag,Queue,Fun,From}]
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({subscribe,Queue,Fun}, From,
            #state{channel=C,assigned=A,unassigned=U}=S) ->
    case lists:keytake(Queue, 2, A) of
        {value, Tuple, Rest} ->
            {reply, ignore, assign([Tuple|Rest],S)};
        false ->
            case ergen_amqp:subscribe(C, Queue) of
                {ok, ConsumerTag} ->
                    {noreply, unassign([{ConsumerTag,Queue,Fun,From}|U],S)};
                {error, Reason} ->
                    {reply, {error,Reason}, S}
            end
    end;
handle_call({cancel,Queue}, From,
            #state{channel=C,assigned=A,unassigned=U}=S) ->
    case lists:keytake(Queue, 2, A) of
        {value, {T,Q,H}, Rest} ->
            case ergen_amqp:cancel(C, T) of
                ok ->
                    {noreply, assign(Rest,unassign([{T,Q,H,From}|U],S))};
                {error, Reason} ->
                    {reply, {error,Reason}, assign([{T,Q,H}|Rest],S)}
            end;
        false ->
            {reply, {error,{badmatch,Queue}}, S}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(#'basic.consume_ok'{consumer_tag=T}, #state{assigned=A,unassigned=U}=S) ->
    case lists:keytake(T, 1, U) of
        {value, {T,Q,H,F}, Rest} ->
            gen_server:reply(F, ok),
            {noreply, assign([{T,Q,H}|A],unassign(Rest,S))};
        false ->
            {stop, {shutdown,{badmatch,T}}, S}
    end;
handle_info(#'basic.cancel_ok'{consumer_tag=T}, #state{unassigned=U}=S) ->
    case lists:keytake(T, 1, U) of
        {value, {_,_,_,F}, Rest} ->
            gen_server:reply(F, ok),
            {noreply, unassign(Rest,S)};
        false ->
            {stop, {shutdown,{badmatch,T}}, S}
    end;
handle_info({#'basic.deliver'{consumer_tag=T,delivery_tag=D},
             #amqp_msg{props=B,payload=P}},
            #state{channel=C,assigned=A}=S) ->
    case lists:keytake(T, 1, A) of
        {value, {T,Q,H}, Rest} ->
            #'P_basic'{correlation_id=I,reply_to=R} = B,
            Result = H(binary_to_term(P)),
            if is_binary(R) ->
                    Props = #'P_basic'{correlation_id = I},
                    Content = #amqp_msg{props = Props, payload = term_to_binary(Result)},
                    ergen_amqp:reply(C, R, Content);
               true ->
                    ok
            end,
            ok = ergen_amqp:ack(C, D),
            {noreply, assign([{T,Q,H}|Rest],S)};
        false ->
            {stop, {shutdown,{badmatch,T}}, S}
    end;
handle_info({'DOWN',_MRef,process,P,Info}, #state{channel=P}=S) ->
    {stop, Info, S#state{channel = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

-spec cleanup(state()) -> ok.
cleanup(#state{}) ->
    %%io:format("~p [~p:cleanup]~n", [self(),?MODULE]),
    ergen_util:flush().

-spec setup([any()]) -> {ok,state()}|{stop,_}.
setup([Channel]) ->
    %%io:format("~p [~p:setup]~n", [self(),?MODULE]),
    process_flag(trap_exit, true),
    monitor(process, Channel),
    {ok, #state{channel = Channel}};
setup(_) ->
    {stop, badarg}.

-spec assign([any()],state()) -> state().
assign(List, #state{}=S) ->
    S#state{assigned = List}.

-spec unassign([any()],state()) -> state().
unassign(List, #state{}=S) ->
    S#state{unassigned = List}.
