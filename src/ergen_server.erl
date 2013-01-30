-module(ergen_server).

-export([start_link/3, stop/1]).
-export([call/2, cast/2]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include("ergen.hrl").

%% == public ==

-spec start_link(atom(),[any()],[non_neg_integer()]) -> {ok,pid()}|{error,_}.
start_link(Type, Args, Group)
  when is_atom(Type), is_list(Args), is_list(Group) ->
    Module = list_to_atom("ergen_" ++ atom_to_list(Type) ++ "_worker"),
    case gen_server:start_link(?MODULE, [Module,Group], []) of
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
call(Pid, Term)
  when is_pid(Pid) ->
    gen_server:call(Pid, {call,Term}, infinity).

-spec cast(pid(),any()) -> ok.
cast(Pid, Term)
  when is_pid(Pid) ->
    gen_server:cast(Pid, {cast,Term}).

%% == behaviour: gen_server ==

-record(state, {
          module :: atom(),
          group :: [non_neg_integer()],
          connection :: pid(),
          sup :: pid()
         }).

-type(state() :: #state{}).

init(Args) ->
    setup(Args).

terminate(_Reason, State) ->
    cleanup(State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({call,Args}, _From, #state{module=M,sup=P}=S) ->
    case ergen_util:choose(list_children(P)) of
        undefined ->
            {reply, {error,badarg}, S};
        Pid ->
            {reply, apply(M, call, [Pid,Args]), S}
    end;
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

handle_cast({cast,Args}, #state{module=M,sup=P}=S) ->
    _ = [ apply(M, cast, [Pid,Args]) || Pid <- list_children(P) ],
    {noreply, S};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN',_MRef,process,P,Info}, #state{connection=P}=S) ->
    {stop, Info, S#state{connection = undefined}};
handle_info({'EXIT',P,Reason}, #state{sup=P}=S) ->
    {stop, Reason, S#state{sup = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% == private: state ==

-spec cleanup(state()) -> ok.
cleanup(#state{sup=P}=S)
  when is_pid(P) ->
    _ = ergen_transient_simple_sup:stop(P),
    cleanup(S#state{sup = undefined});
cleanup(#state{connection=P}=S)
  when is_pid(P) ->
    _ = ergen_amqp:disconnect(P),
    cleanup(S#state{connection = undefined});
cleanup(#state{}) ->
    %%io:format("~p [~p:cleanup]~n", [self(),?MODULE]),
    ergen_util:flush().

-spec setup([any()]) -> {ok,state()}|{stop,_}.
setup([Module,Group]) ->
    %%io:format("~p [~p:setup] module=~p, group=~w~n", [self(),?MODULE,Module,Group]),
    process_flag(trap_exit, true),
    {ok, #state{module = Module, group = Group}};
setup(_) ->
    {stop, badarg}.

-spec setup({atom(),any()},state()) -> state().
setup({connection,Connection}, #state{connection=undefined}=S)
  when is_binary(Connection) ->
    case ergen_amqp:connect(Connection) of
        {ok, Pid} ->
            monitor(process, Pid),
            S#state{connection = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({options,Options}, #state{module=M,group=G,connection=X,sup=undefined}=S)
  when is_list(Options) ->
    L = [ {connection,X} | proplists:delete(connection,Options) ],
    case ergen_transient_simple_sup:start_link(M, [L], G) of
        {ok, Pid} ->
            S#state{sup = Pid};
        {error, Reason} ->
            throw({Reason,S})
    end;
setup({K,_}, #state{}=S) ->
    throw({{badmatch,K},S}).

%% == private ==

-spec list_children(sup_ref()) -> [pid()].
list_children(SupRef) ->
    ergen_util:filter(fun is_pid/1, 2, supervisor:which_children(SupRef)).
