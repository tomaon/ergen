-module(ergen_mee_worker).

-export([start_link/2, stop/1]).
-export([call/2, call/3, cast/2]).

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
call(Pid, set_base_time) ->
    call(Pid, ?CMD_MEE_SET_BASE_TIME, []);
call(Pid, {submit_trade_result,{'ergen.MEE.TM',Term}}) ->
    call(Pid, ?CMD_MEE_SUBMIT_TRADE_RESULT, Term);
call(Pid, generate_trade_result) ->
    call(Pid, ?CMD_MEE_GENERATE_TRADE_RESULT, []);
call(Pid, disable_ticker_tape) ->
    call(Pid, ?CMD_MEE_DISABLE_TICKER_TAPE, []);
call(Pid, enable_ticker_tape) ->
    call(Pid, ?CMD_MEE_ENABLE_TICKER_TAPE, []).

-spec call(pid(),integer(),any()) -> any().
call(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command) ->
    gen_server:call(Pid, {call,Command,Args}, infinity).

-spec cast(pid(),any()) -> any().
cast(Pid, set_base_time) ->
    call(Pid, ?CMD_MEE_SET_BASE_TIME, []);
cast(Pid, disable_ticker_tape) ->
    call(Pid, ?CMD_MEE_DISABLE_TICKER_TAPE, []);
cast(Pid, enable_ticker_tape) ->
    call(Pid, ?CMD_MEE_ENABLE_TICKER_TAPE, []).

%% == behaviour: gen_server ==

-record(state, {
          id :: non_neg_integer(),
          channel :: pid(),
          client :: pid(),
          exchange = <<>> :: binary(),
          logger = <<>> :: binary(),
          port :: pid(),
          server :: pid(),
          tag :: reference()
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({call,_,_}=R, From, #state{id=I,port=undefined}=S) ->
    case ergen_sup:start_port(mee, I) of
        {ok, Pid} ->
            handle_call(R, From, S#state{port = Pid});
        {error, Reason} ->
            {stop, {error,Reason}, S}
    end;
handle_call({call,Command,Args}, {_,Tag}, #state{}=S)
  when ?CMD_MEE_SUBMIT_TRADE_RESULT =:= Command;
       ?CMD_MEE_GENERATE_TRADE_RESULT =:= Command ->
    {Time, Value} = timer:tc(fun handle_run/3, [Command,Args,S]),
    %%io:format("~p [~p:call] ~p=~p, ~pus~n", [self(),?MODULE,Command,Value,Time]),
    handle_logger({Command,Value,Time}, S),
    if
        is_integer(Value), Value > 0 ->
            {ok, _} = timer:send_after(Value, {timeout,Tag}),
            {reply, Value, S#state{tag = Tag}};
        true ->
            {reply, Value, S}
    end;
handle_call({call,Command,Args}, _From, #state{port=P}=S) ->
    {reply, ergen_port:call(P, Command, Args), S};
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

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout,T}, #state{tag=T}=S) ->
    Self = self(),
    _ = spawn(fun() -> call(Self, generate_trade_result) end),
    {noreply, S#state{tag = undefined}};
handle_info({timeout,_}, #state{}=S) ->
    {noreply, S#state{tag = undefined}};
handle_info({#'basic.return'{reply_code=C,reply_text=T},_}, #state{}=S) ->
    {stop, {error,{C,T}}, S}; % TODO
handle_info({'DOWN',_MRef,process,P,Info}, #state{channel=P}=S) ->
    {stop, Info, S#state{channel = undefined}};
handle_info({'EXIT',P,Reason}, #state{client=P}=S) ->
    {stop, Reason, S#state{client = undefined}};
handle_info({'EXIT',P,Reason}, #state{port=P}=S) ->
    {stop, Reason, S#state{port = undefined}};
handle_info({'EXIT',P,Reason}, #state{server=P}=S) ->
    {stop, Reason, S#state{server = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: callback ==

-spec handle_run(integer(),any(),state()) -> any().
handle_run(Command, Args, #state{port=P}=S) ->
    case ergen_port:call(P, Command, Args, fun handle_port/1) of
        Term when is_list(Term) ->
            ergen_util:do_while(fun handle_publish/2, Term, S);
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_port(any()) -> {reply,any()}.
handle_port(Term) when is_integer(Term) ->
    {reply, Term};
handle_port({'ergen.BH.MF',_}=T) ->
    {noreply, T};
handle_port({'ergen.BH.TR',_}=T) ->
    {noreply, T};
handle_port({error,_}=T) ->
    {reply, T}.

-spec handle_publish(any(),state()) -> boolean()|integer().
handle_publish(Term, #state{}) when is_integer(Term) ->
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
cleanup(#state{server=P}=S)
  when is_pid(P) ->
    _ = ergen_rpc_server:stop(P),
    cleanup(S#state{server = undefined});
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
setup({subscribe,Args}, #state{channel=C,server=undefined}=S)
  when is_list(Args), is_pid(C) ->
    case ergen_rpc_server:start_link(C) of
        {ok, Pid} ->
            lists:foldl(fun setup_server/2, S#state{server = Pid}, Args);
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({logger,Logger}, #state{}=S)
  when is_binary(Logger) ->
    S#state{logger = Logger};
setup({K,_}, #state{}=S) ->
    throw({{badmatch,K},S}).

-spec setup_server(binary(),state()) -> state().
setup_server(Queue, #state{server=P}=S)
  when is_binary(Queue), undefined =/= P ->
    Self = self(),
    Fun = fun(Term) -> call(Self, {submit_trade_result,Term}) end,
    case ergen_rpc_server:subscribe(P, Queue, Fun) of
        ok ->
            S;
        {error, Reason} ->
            throw({Reason,S})
    end;
setup_server(_Args, #state{}=S) ->
    throw({badarg,S}).
