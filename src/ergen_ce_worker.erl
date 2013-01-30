-module(ergen_ce_worker).

-export([start_link/2, stop/1]).
-export([call/2, call/3, cast/2, cast/4]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("ergen.hrl").

%% == public ==

-spec start_link([any()],non_neg_integer()) -> {ok,pid()}|{error,_}.
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

-spec call(pid(),any()) -> any().
call(Pid, do_txn) ->
    call(Pid, ?CMD_CE_DO_TXN, []).

-spec call(pid(),integer(),any()) -> any().
call(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command) ->
    gen_server:call(Pid, {call,Command,Args}, infinity).

-spec cast(pid(),any()) -> ok.
cast(Pid, {do_txn,Interval}) ->
    cast(Pid, ?CMD_CE_DO_TXN, [], Interval).

-spec cast(pid(),integer(),any(),integer()) -> ok.
cast(Pid, Command, Args, Interval)
  when is_pid(Pid), is_integer(Command), is_integer(Interval) ->
    gen_server:cast(Pid, {cast,Command,Args,Interval}).

%% == behaviour: gen_server ==

-record(state, {
          id :: non_neg_integer(),
          channel :: pid(),
          client :: pid(),
          exchange = <<>> :: binary(),
          logger = <<>> :: binary(),
          port :: pid(),
          tref :: timer:tref()
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({call,Command,Args}, _From, #state{}=S) ->
    {Time, {Value,State}} = timer:tc(fun handle_run/3, [Command,Args,S]),
    %%io:format("~p [~p:call] ~p=~p, ~pus~n", [self(),?MODULE,Command,Value,Time]),
    handle_logger({Command,Value,Time}, State),
    {reply, Value, State};
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

handle_cast({cast,_Command,_Args,Interval}, #state{tref=T}=S)
  when Interval =< 0, undefined =/= T ->
    {ok, cancel} = timer:cancel(T),
    {noreply, S#state{tref = undefined}};
handle_cast({cast,Command,Args,Interval}, #state{}=S)
  when Interval > 0 ->
    {ok, TRef} = timer:send_after(Interval, {run,Command,Args,Interval}),
    {noreply, S#state{tref = TRef}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({run,Command,Args,Interval}=I, #state{}=S) ->
    {Time, {Value,State}} = timer:tc(fun handle_run/3, [Command,Args,S]),
    %%io:format("~p [~p:run] ~p=~p, ~pus~n", [self(),?MODULE,Command,Value,Time]),
    handle_logger({Command,Value,Time}, State),
    {ok, TRef} = timer:send_after(Interval, I),
    {noreply, State#state{tref = TRef}};
handle_info({#'basic.return'{reply_code=C,reply_text=T},_}, #state{}=S) ->
    {stop, {error,{C,T}}, S}; % TODO
handle_info({'DOWN',_MRef,process,P,Info}, #state{channel=P}=S) ->
    {stop, Info, S#state{channel = undefined}};
handle_info({'EXIT',P,Reason}, #state{client=P}=S) ->
    {stop, Reason, S#state{client = undefined}};
handle_info({'EXIT',P,Reason}, #state{port=P}=S) ->
    {stop, Reason, S#state{port = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: callback ==

-spec handle_run(integer(),any(),state()) -> {any(),state()}.
handle_run(Command, Args, #state{id=I,port=undefined}=S) ->
    case ergen_sup:start_port(ce, I) of
        {ok, Pid} ->
            handle_run(Command, Args, S#state{port = Pid});
        {error, Reason} ->
            {error,Reason}
    end;
handle_run(Command, Args, #state{port=P}=S) ->
    case ergen_port:call(P, Command, Args, fun handle_port/1) of
        Term when is_list(Term) ->
            {ergen_util:do_while(fun handle_publish/2, Term, S), S};
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_port(any()) -> {reply,any()}.
handle_port(Term) when is_boolean(Term) ->
    {reply, Term};
handle_port({'ergen.BH.BV',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.CP',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.MW',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.SD',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.TL',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.TO',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.TS',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.TU',_}=T) ->
    {noreply, T};
handle_port({error,_}=T) ->
    {reply, T}.

-spec handle_publish(any(),state()) -> boolean().
handle_publish(Term, #state{}) when is_boolean(Term) ->
    Term;
handle_publish({error,_}, #state{}) ->
    false;
handle_publish({K,_}=T, #state{client=P,exchange=E}) ->
    case ergen_rpc_client:publish(P, E, atom_to_binary(K,latin1), term_to_binary(T)) of
        Term when is_boolean(Term) ->
            Term;
        {error, _} ->
            false
    end.

-spec handle_logger(any(),state()) -> ok.
handle_logger(Term, #state{channel=C,logger=L}) ->
    ergen_amqp:publish(C, L, <<"ergen.LG">>, term_to_binary(Term)).

%% == private: state ==

-spec cleanup(state()) -> ok.
cleanup(#state{port=P}=S)
  when is_pid(P) ->
    cleanup(S#state{port = undefined});
cleanup(#state{client=P}=S)
  when is_pid(P) ->
    _ = ergen_rpc_client:stop(P),
    cleanup(S#state{client = undefined});
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
setup({exchange,Exchange}, #state{channel=C,client=undefined}=S)
  when is_binary(Exchange), is_pid(C) ->
    case ergen_rpc_client:start_link(C) of
        {ok, Pid} ->
            S#state{client = Pid, exchange = Exchange};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({logger,Logger}, #state{}=S)
  when is_binary(Logger) ->
    S#state{logger = Logger};
setup({K,_}, #state{}=S) ->
    throw({{badmatch,K},S}).
