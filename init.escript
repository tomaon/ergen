#!/usr/bin/env escript
%% -*- erlang -*-

-include_lib("amqp_client/include/amqp_client.hrl").

main(_) ->
    M = ergen_amqp,
    case M:connect("amqp://localhost") of
        {ok, Connection} ->
            case M:open(Connection,{amqp_direct_consumer,[self()]}) of
                {ok, Channel} ->
                    L = [
                         %% -- --
                         {declare_exchange, [<<"ergen.fanout">>,<<"fanout">>]},
                         {declare_queue, [<<"ergen.LG">>]},
                         {bind_queue, [<<"ergen.fanout">>,<<"ergen.LG">>,<<"ergen.LG">>]},
                         %% -- --
                         {declare_exchange, [<<"ergen.topic">>,<<"topic">>]},
                         {declare_queue, [<<"ergen.BH">>]},
                         {bind_queue, [<<"ergen.topic">>,<<"ergen.BH">>,<<"ergen.BH.*">>]},
                         {declare_queue, [<<"ergen.MEE">>]},
                         {bind_queue, [<<"ergen.topic">>,<<"ergen.MEE">>,<<"ergen.MEE.*">>]}
                        ],
                    _ = [ apply(M,F,[Channel|A]) || {F,A} <- L ],
                    M:close(Channel)
            end,
            M:disconnect(Connection)
    end.
