-module(ergen_rpc_client).

-export([start_link/1, stop/1]).
-export([publish/4, publish/5, publish/6]).

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

-spec publish(pid(),binary(),binary(),binary()) -> any().
publish(Pid, Exchange, RoutingKey, Payload)
  when is_pid(Pid), is_binary(Exchange), is_binary(RoutingKey), is_binary(Payload) ->
    publish(Pid, Exchange, RoutingKey, Payload, 10000). % default=5000

-spec publish(pid(),binary(),binary(),binary(),non_neg_integer()) -> any().
publish(Pid, Exchange, RoutingKey, Payload, Timeout)
  when is_pid(Pid), is_binary(Exchange), is_binary(RoutingKey), is_binary(Payload) ->
    publish(Pid, Exchange, RoutingKey, Payload, Timeout, 1).

-spec publish(pid(),binary(),binary(),binary(),non_neg_integer(),integer()) -> any().
publish(Pid, Exchange, RoutingKey, Payload, Timeout, Retry)
  when is_pid(Pid), is_binary(Exchange), is_binary(RoutingKey), is_binary(Payload),
       is_integer(Timeout), Timeout > 0, is_integer(Retry), Retry > 0 ->
    try gen_server:call(Pid, {publish,Exchange,RoutingKey,Payload,Timeout}, infinity) of
        {error, timeout} ->
            publish(Pid, Exchange, RoutingKey, Payload, Timeout, Retry - 1);
        Term ->
            Term
    catch
        exit:{timeout,_} -> % NOT_REACHED
            {error,timeout}
    end;
publish(Pid, _Exchange, _RoutingKey, _Payload, _Timeout, Retry)
  when is_integer(Retry), Retry =< 0 ->
    error_logger:error_report(timeout), % TODO
    erlang:process_display(Pid, backtrace),
    io:format("~p [~p:publish] TIMEOUT!~n", [self(),?MODULE]),
    {error, timeout}.

%% == behaviour: gen_server ==

-record(state, {
          channel :: pid(),
          assigned   = [] :: [any()], % [{CorrelationId,From,Queue}]
          unassigned = [] :: [any()]  % [Queue]
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({publish,Exchange,RoutingKey,Payload,_Timeout}, {_Pid,Tag}=F,
            #state{channel=C,assigned=A}=S) ->
    case declare(S) of
        {ok, Q, Rest} ->
            CorrelationId = term_to_binary(Tag),
            Props = #'P_basic'{correlation_id = CorrelationId, reply_to = Q},
            Content = #amqp_msg{props = Props, payload = Payload},
            case ergen_amqp:publish(C, Exchange, RoutingKey, Content) of
                ok ->
                    %%{ok, _} = timer:send_after(Timeout, {timeout,Tag}), TODO
                    {noreply, assign([{CorrelationId,F,Q}|A],unassign(Rest,S))};
                {error, Reason} ->
                    ok = delete(Q, S),
                    {reply, {error,Reason}, unassign(Rest,S)}
            end;
        {error, Reason} ->
            {stop, {shutdown,Reason}, S}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error,badarg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({#'basic.deliver'{consumer_tag=T,delivery_tag=D},
             #amqp_msg{props=B,payload=P}},
            #state{channel=C,assigned=A,unassigned=U}=S) ->
    case lists:keytake(I = B#'P_basic'.correlation_id, 1, A) of
        {value, {I,F,Q}, Rest} ->
            gen_server:reply(F, binary_to_term(P)),
            ok = ergen_amqp:ack(C, D),
            {noreply, assign(Rest,unassign([Q|U],S))};
        false ->
            {stop, {shutdown,{badmatch,T}}, S}
    end;
handle_info({timeout,Tag}, #state{assigned=A}=S) ->
    case lists:keytake(I = term_to_binary(Tag), 1, A) of
        {value, {I,F,Q}, Rest} ->
            gen_server:reply(F, {error,timeout}),
            ok = delete(Q, S),
            {noreply, assign(Rest,S)};
        false ->
            {noreply, S} % IGNORE
    end;
handle_info(#'basic.consume_ok'{}, #state{}=S) ->
    {noreply, S};
handle_info(#'basic.cancel_ok'{}, #state{}=S) ->
    {noreply, S};
handle_info({'DOWN',_MRef,process,P,Info}, #state{channel=P}=S) ->
    {stop, Info, S#state{channel = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

-spec cleanup(state()) -> ok.
cleanup(#state{channel=P,unassigned=L}=S)
  when is_pid(P), is_list(L), length(L) > 0 ->
    _ = [ ok = delete(E,S) || E <- L ],
    cleanup(S#state{unassigned = []});
cleanup(#state{channel=P,assigned=L}=S)
  when is_pid(P), is_list(L), length(L) > 0 ->
    _ = [ ok = delete(E,S) || {_,_,E} <- L ],
    cleanup(S#state{assigned = []});
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

-spec declare(state()) -> {ok,binary(),[binary()]}|{error,_}.
declare(#state{channel=C,unassigned=[]}) ->
    case ergen_amqp:declare_queue(C, <<>>) of
        {ok, Queue} ->
            case ergen_amqp:subscribe(C, Queue) of
                {ok, _ConsumerTag} ->
                    {ok, Queue, []};
                {error, Reason} ->
                    ok = ergen_amqp:delete_queue(C, Queue),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
declare(#state{unassigned=[H|T]}) ->
    {ok, H, T}.

-spec delete(binary(),state()) -> ok|{error,_}.
delete(Queue, #state{channel=C}) ->
    %% cancel, delete(if-unused,if-empty), get? : TODO
    ergen_amqp:delete_queue(C, Queue).
