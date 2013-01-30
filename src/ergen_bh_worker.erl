-module(ergen_bh_worker).

-export([start_link/2, stop/1]).
-export([call/2, call/3]).

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
call(Pid, {do_txn,{'ergen.BH.BV',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_BV, Term);
call(Pid, {do_txn,{'ergen.BH.CP',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_CP, Term);
call(Pid, {do_txn,{'ergen.BH.DM',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_DM, Term);
call(Pid, {do_txn,{'ergen.BH.MF',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_MF, Term);
call(Pid, {do_txn,{'ergen.BH.MW',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_MW, Term);
call(Pid, {do_txn,{'ergen.BH.SD',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_SD, Term);
call(Pid, {do_txn,{'ergen.BH.TC',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_TC, Term);
call(Pid, {do_txn,{'ergen.BH.TL',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_TL, Term);
call(Pid, {do_txn,{'ergen.BH.TO',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_TO, Term);
call(Pid, {do_txn,{'ergen.BH.TR',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_TR, Term);
call(Pid, {do_txn,{'ergen.BH.TS',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_TS, Term);
call(Pid, {do_txn,{'ergen.BH.TU',Term}}) ->
    call(Pid, ?CMD_BH_DO_TXN_TU, Term).

-spec call(pid(),integer(),any()) -> any().
call(Pid, Command, Args)
  when is_pid(Pid), is_integer(Command) ->
    gen_server:call(Pid, {call,Command,Args}, infinity).

%% == behaviour: gen_server ==

-record(state, {
          id :: non_neg_integer(),
          channel :: pid(),
          exchange = <<>> :: binary(),
          logger = <<>> :: binary(),
          port :: pid(),
          server :: pid()
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

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({#'basic.return'{reply_code=C,reply_text=T},_}, #state{}=S) ->
    {stop, {error,{C,T}}, S}; % TODO
handle_info({'DOWN',_MRef,process,P,Info}, #state{channel=P}=S) ->
    {stop, Info, S#state{channel = undefined}};
handle_info({'EXIT',P,Reason}, #state{port=P}=S) ->
    {stop, Reason, S#state{port = undefined}};
handle_info({'EXIT',P,Reason}, #state{server=P}=S) ->
    {stop, Reason, S#state{server = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: callback ==

-spec handle_run(integer(),any(),state()) -> {any(),state()}.
handle_run(Command, Args, #state{id=I,port=undefined}=S) ->
    case ergen_sup:start_port(bh, I) of
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
handle_port({'ergen.MEE.TM',_}=T) ->
    {noreply, T};
handle_port({error,_}=T) ->
    {reply, T}.

-spec handle_publish(any(),state()) -> boolean().
handle_publish(Term, #state{}) when is_boolean(Term) ->
    Term;
handle_publish({error,_}, #state{}) ->
    false;
handle_publish({K,_}=T, #state{channel=C,exchange=E}) ->
    case ergen_amqp:publish(C, E, atom_to_binary(K,latin1), term_to_binary(T)) of
        ok ->
            true;
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
setup({exchange,Exchange}, #state{}=S)
  when is_binary(Exchange) ->
    S#state{exchange = Exchange};
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
    Fun = fun(Term) -> call(Self, {do_txn,Term}) end,
    case ergen_rpc_server:subscribe(P, Queue, Fun) of
        ok ->
            S;
        {error, Reason} ->
            throw({Reason,S})
    end;
setup_server(_Args, #state{}=S) ->
    throw({badarg,S}).
