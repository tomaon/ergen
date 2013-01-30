-module(ergen_transient_simple_sup).

-export([start_link/3, stop/1]).
-export([delegate/2]).

-behaviour(supervisor).
-export([init/1]).

-include("ergen.hrl").

%% == public ==

-spec start_link(module(),[any()],[any()]) -> {ok,pid()}|{error,_}.
start_link(Module, Args1, Args2)
  when is_atom(Module), is_list(Args1), is_list(Args2) ->
    case supervisor:start_link(?MODULE, [Module,Args1]) of
        {ok, Pid} ->
            try lists:foldl(fun start_child/2, Pid, Args2) of
                Pid ->
                    {ok, Pid}
            catch
                Reason ->
                    S = {?MODULE, start_link, {module,Module}},
                    error_logger:error_report([S,Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(sup_ref()) -> ok|{error,_}.
stop(SupRef)
  when is_atom(SupRef);is_pid(SupRef) ->
    try lists:foldl(fun terminate_child/2, SupRef, list_children(SupRef)) of
        SupRef ->
            ok
    catch
        Reason ->
            {error, Reason}
    end.

-spec delegate(sup_ref(),{atom(),atom(),any()}) -> any().
delegate(SupRef, {M,F,A})
  when (is_atom(SupRef) or is_pid(SupRef)), is_atom(M), is_atom(F) ->
    delegate(SupRef, M, F, A).

%% == behaviour: supervisor ==

init([Module,Args]) ->
    %%io:format("~p [~p:init] module=~p~n", [self(),?MODULE,Module]),
    L = [
         {Module,
          {Module,start_link,Args},
          transient,
          3000,
          worker,
          [Module]}
        ],
    {ok, {{simple_one_for_one,0,1},L}}.

%% == private ==

-spec delegate(sup_ref(),atom(),atom(),any()) -> any().
delegate(SupRef, M, call, A) ->
    case ergen_util:choose(list_children(SupRef)) of
        undefined ->
            {error, badarg};
        Pid ->
            apply(M, call, [Pid,A])
    end;
delegate(SupRef, M, cast, A) ->
    _ = [ apply(M, cast, [Pid,A]) || Pid <- list_children(SupRef) ],
    ok.

-spec list_children(sup_ref()) -> [pid()].
list_children(SupRef) ->
    ergen_util:filter(fun is_pid/1, 2, supervisor:which_children(SupRef)).

-spec start_child(any(),sup_ref()) -> sup_ref().
start_child(Args, SupRef) ->
    case supervisor:start_child(SupRef, [Args]) of
        {ok, _Pid} ->
            SupRef;
        {ok, _Pid, _Info} ->
            SupRef;
        {error, Reason} ->
            throw(Reason)
    end.

-spec terminate_child(pid(),sup_ref()) -> sup_ref().
terminate_child(Pid, SupRef) ->
    case supervisor:terminate_child(SupRef, Pid) of
        ok ->
            SupRef;
        {error, Reason} ->
            throw(Reason)
    end.
