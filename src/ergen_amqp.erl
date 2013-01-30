-module(ergen_amqp).

-export([connect/1, disconnect/1]).

-export([open/2, close/1]).
-export([ack/2]).
-export([subscribe/2, cancel/2]).
-export([publish/4, reply/3]).

-export([declare_exchange/3]).
-export([bind_queue/4, declare_queue/2, delete_queue/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% == public: connection ==

-spec connect(string()|binary()) -> {ok,pid()}|{error,_}.
connect(Uri)
  when is_list(Uri) ->
    case amqp_uri:parse(Uri) of
        {ok, AmqpParams} ->
            amqp_connection:start(AmqpParams);
        {error, Reason} ->
            {error, Reason}
    end;
connect(Uri)
  when is_binary(Uri) ->
    connect(binary_to_list(Uri)).

-spec disconnect(pid()) -> ok.
disconnect(Connection)
  when is_pid(Connection) ->
    amqp_connection:close(Connection),
    ok.

%% == public: channel ==

-spec open(pid(),{atom(),[any()]}) -> {ok,pid()}|{error,_}.
open(Connection, Consumer)
  when is_pid(Connection) ->
    try amqp_connection:open_channel(Connection, Consumer) of
        {ok, Pid} ->
            {ok, Pid};
        closing ->
            {error, closing};
        {error, Reason} ->
            {error, Reason}
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec close(pid()) -> ok.
close(Channel)
  when is_pid(Channel) ->
    try amqp_channel:close(Channel) of
        _ ->
            ok
    catch
        _:Reason ->
            {error, Reason}
    end.

-spec ack(pid(),any()) -> ok|{error,_}.
ack(Channel, DeliveryTag)
  when is_pid(Channel) ->
    Method = #'basic.ack'{delivery_tag = DeliveryTag}, % multiple?
    cast(Channel, Method).

-spec subscribe(pid(),binary()) -> {ok,binary()}|{error,_}.
subscribe(Channel, Queue)
  when is_pid(Channel), is_binary(Queue) ->
    Method = #'basic.consume'{queue = Queue}, % nowait=false
    case call(Channel, Method) of
        #'basic.consume_ok'{consumer_tag=T} ->
            {ok, T};
        Reason ->
            {error, Reason}
    end.

-spec cancel(pid(),any()) -> ok|{error,_}.
cancel(Channel, ConsumerTag)
  when is_pid(Channel) ->
    Method = #'basic.cancel'{consumer_tag = ConsumerTag}, % nowait=false
    case call(Channel, Method) of
        #'basic.cancel_ok'{} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec publish(pid(),binary(),binary(),any()) -> ok|{error,_}.
publish(Channel, Exchange, RoutingKey, Content)
  when is_pid(Channel), is_binary(Exchange), is_binary(RoutingKey),
       is_record(Content,'amqp_msg') ->
    Method = #'basic.publish'{exchange = Exchange, routing_key = RoutingKey},
    cast(Channel, Method, Content);
publish(Channel, Exchange, RoutingKey, Content)
  when is_binary(Content) ->
    publish(Channel, Exchange, RoutingKey, #amqp_msg{payload = Content}).

-spec reply(pid(),binary(),tuple()) -> ok|{error,_}.
reply(Channel, RoutingKey, Content)
  when is_pid(Channel), is_binary(RoutingKey),
       is_record(Content,'amqp_msg') ->
    Method = #'basic.publish'{routing_key = RoutingKey},
    %%mandatory = true, immediate = true},
    cast(Channel, Method, Content).

%% == public: exchange ==

-spec declare_exchange(pid(),binary(),binary()) -> ok|{error,_}.
declare_exchange(Channel, Exchange, Type)
  when is_pid(Channel), is_binary(Exchange), is_binary(Type) ->
    Method = #'exchange.declare'{exchange = Exchange, type = Type},
    case call(Channel, Method) of
        #'exchange.declare_ok'{} ->
            ok;
        Reason ->
            {error, Reason}
    end.

%% == public: queue ==

-spec bind_queue(pid(),binary(),binary(),binary()) -> ok|{error,_}.
bind_queue(Channel, Exchange, Queue, RoutingKey)
  when is_pid(Channel), is_binary(Queue) ->
    Method = #'queue.bind'{exchange = Exchange, queue = Queue, routing_key = RoutingKey},
    case call(Channel, Method) of
        #'queue.bind_ok'{} ->
            ok;
        Reason ->
            {error, Reason}
    end.

-spec declare_queue(pid(),binary()) -> {ok,binary()}|{error,_}.
declare_queue(Channel, Queue)
  when is_pid(Channel), is_binary(Queue) ->
    Method = #'queue.declare'{queue = Queue},
    case call(Channel, Method) of
        #'queue.declare_ok'{queue=Q} ->
            {ok, Q};
        Reason ->
            {error, Reason}
    end.

-spec delete_queue(pid(),binary()) -> ok|{error,_}.
delete_queue(Channel, Queue)
  when is_pid(Channel), is_binary(Queue) ->
    Method = #'queue.delete'{queue = Queue},
    case call(Channel, Method) of
        #'queue.delete_ok'{} ->
            ok;
        Reason ->
            {error, Reason}
    end.

%% == private ==

-spec call(pid(),tuple()) -> any().
call(Channel, Method) ->
    %%io:format("~p [~p:call/2] ~p~n", [self(),?MODULE,Method]),
    try amqp_channel:call(Channel, Method)
    catch
        _:Reason ->
            Reason
    end.

-spec cast(pid(),tuple()) -> any().
cast(Channel, Method) ->
    %%io:format("~p [~p:cast/2] ~p~n", [self(),?MODULE,Method]),
    try amqp_channel:cast(Channel, Method)
    catch
        _:Reason ->
            Reason
    end.

-spec cast(pid(),tuple(),tuple()) -> any().
cast(Channel, Method, Content) ->
    %%io:format("~p [~p:cast/3] ~p~n", [self(),?MODULE,Method]),
    try amqp_channel:cast(Channel, Method, Content)
    catch
        _:Reason ->
            Reason
    end.
