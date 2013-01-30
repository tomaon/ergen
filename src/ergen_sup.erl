-module(ergen_sup).

-export([start_link/4, stop/1]).
-export([call/2, cast/2]).

-export([start_port/2]).

-behaviour(supervisor).
-export([init/1]).

-include("ergen.hrl").

%% == public ==

-spec start_link(atom(),[any()],[any()],[[non_neg_integer()]]) -> {ok,pid()}|{error,_}.
start_link(Type, PortArgs, ServerArgs, Groups)
  when is_atom(Type), is_list(PortArgs), is_list(ServerArgs), is_list(Groups) ->
    SupName = {local, name(Type)},
    supervisor:start_link(SupName, ?MODULE, [Type,PortArgs,ServerArgs,Groups]).

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

-spec call(atom(),any()) -> any().
call(Type, Term)
  when is_atom(Type) ->
    delegate(name(Type), ergen_server_sup, {ergen_server,call,Term}).

-spec cast(atom(),any()) -> any().
cast(Type, Term)
  when is_atom(Type) ->
    delegate(name(Type), ergen_server_sup, {ergen_server,cast,Term}).

%% -- --

-spec start_port(atom(),non_neg_integer()) -> {ok,pid()}|{error,_}.
start_port(Type, Id)
  when is_atom(Type), is_integer(Id), Id > 0 ->
    case find_child(name(Type), ergen_port_sup) of
        undefined ->
            {error, badarg};
        {C,_} ->
            supervisor:start_child(C, [Id])
    end.

%% == behaviour: supervisor ==

init([Type,PortArgs,ServerArgs,Groups]) ->
    %%io:format("~p [~p:init] type=~p, groups=~w~n", [self(),?MODULE,Type,Groups]),
    L = [
         %% -- port --
         {ergen_port_sup,
          {ergen_transient_simple_sup,start_link,[ergen_port,PortArgs,[]]},
          permanent,
          3000,
          supervisor,
          [ergen_transient_simple_sup]},
         %% -- server --
         {ergen_server_sup,
          {ergen_transient_simple_sup,start_link,[ergen_server,[Type|ServerArgs],Groups]},
          permanent,
          3000,
          supervisor,
          [ergen_transient_simple_sup]}
        ],
    {ok, {{rest_for_one,10,10},L}}.

%% == private ==

-spec delegate(sup_ref(),atom(),tuple()) -> any().
delegate(SupRef, Id, Tuple) ->
    case find_child(SupRef, Id) of
        undefined ->
            {error, badarg};
        {C,M} ->
            apply(M, delegate, [C,Tuple])
    end.

-spec find_child(sup_ref(),atom()) -> {pid(),atom()}|undefined.
find_child(SupRef, Id) ->
    case lists:keyfind(Id, 1, supervisor:which_children(SupRef)) of
        {Id,C,supervisor,[M]} when is_pid(C) ->
            {C,M};
        _ ->
            undefined
    end.

-spec list_children(sup_ref()) -> [atom()].
list_children(SupRef) ->
    [ I || {I,_,supervisor,_} <- supervisor:which_children(SupRef) ].

-spec name(atom()) -> atom().
name(Type) ->
    list_to_atom("ergen_" ++ atom_to_list(Type) ++ "_sup").

-spec terminate_child(atom(),sup_ref()) -> sup_ref().
terminate_child(Id, SupRef) ->
    case find_child(SupRef, Id) of
        undefined ->
            SupRef; % ignore
        {C,M} ->
            apply(M, stop, [C]),
            SupRef
    end.
