#!/usr/bin/env escript
%% -*- erlang -*-
%%! -s folsom

main(_) ->
    %%spawn(fun () -> run(0) end),
    run(1),
    flush().

run(1) ->
    case ergen_amqp:connect("amqp://localhost") of
        {ok, Connection} ->
            L = [
                 {connection, Connection},
                 {subscribe, [<<"ergen.LG">>]}
                ],
            M = ergen_st_worker,
            case M:start_link(L, 9) of
                {ok, Pid} ->
                    io:format("worker: ~p~n", [Pid]),
                    M:cast(Pid, {stat,5000}),
                    %%timer:send_after(3000, timeout),
                    wait();
                %%M:stop(Pid);
                {error, Reason} ->
                    io:format("ERROR: ~p~n", [Reason])
            end,
            ergen_amqp:disconnect(Connection)
    end;
run(0) ->
    M = ergen_amqp,
    case M:connect("amqp://localhost") of
        {ok, Connection} ->
            case M:open(Connection, {amqp_selective_consumer,[]}) of
                {ok, Channel} ->
                    amqp_selective_consumer:register_default_consumer(Channel, self()),
                    run(server, Channel),
                    M:close(Channel)
            end,
            M:disconnect(Connection)
    end.

run(server, Channel) ->
    E = <<"ergen.topic">>,
    Q = <<"ergen.MEE">>,
    K = <<"ergen.MEE.XX">>,
    F1 = fun(T) -> run(Channel, E, K, T) end,
    F2 = fun(T) -> ergen_amqp:publish(Channel, E, K, term_to_binary(T)) end,
    H = fun (P) ->
                io:format("p=~p~n", [P]),
                false
        end,
    M = ergen_rpc_server,
    case M:start_link(Channel) of
        {ok, Pid} ->
            io:format("start~n"),
            wait(1000),
            %%erlang:process_display(Pid, backtrace),
            case M:subscribe(Pid, Q, H) of
                ok ->
                    io:format("subscribe: ok~n"),
                    %%erlang:process_display(Pid, backtrace),
                    wait(1000),
                    io:format("re-subscribe: ~p~n", [M:subscribe(Pid,Q,H)]),
                    %%erlang:process_display(Pid, backtrace),
                    wait(1000),
                    io:format("publish: 1,~p~n", [F1("Hello,World!")]),
                    wait(1000),
                    io:format("publish: 2,~p~n", [F2("Hello,World!")]),
                    wait(1000),
                    io:format("cancel: ~p~n", [M:cancel(Pid,Q)]),
                    %%erlang:process_display(Pid, backtrace),
                    wait(1000)
            end,
            io:format("stop: ~p~n", [M:stop(Pid)])
    end;
run(bh_4, Channel) ->
    K = 'ergen.BH.MF',
    V = [
         {unique_symbols,2},
         {zz_padding1,<<>>},
         {status_and_trade_type,
          {{status_submitted, <<"SBMT">>},
           {type_limit_buy,<<"TLB">>},
           {type_limit_sell,<<"TLS">>},
           {type_stop_loss,<<"TSL">>}}},
         {zz_padding2,<<>>},
         {entries,
          [{ticker_entry,
            {{price_quote,27.63},
             {price_quote,200},
             {symbol,<<"ANN">>}}},
           {ticker_entry,
            {{price_quote,21.34},
             {price_quote,375},
             {symbol,<<"ASFI">>}}}
          ]}
        ],
    run(Channel, K, {K,V});
run(bh_3, Channel) ->
    K = 'ergen.BH.TO',
    V = {
      {trade_type,4},
      {executor_is_account_owner,true},
      [
       {requested_price,28.16},
       {acct_id,43000006473},
       {is_lifo,0},
       {roll_it_back,0},
       {trade_qty,200},
       {type_is_margin,1},
       {co_name,<<>>},
       {exec_f_name,<<"James">>},
       {exec_l_name,<<"Crall">>},
       {exec_tax_id,<<"855LQ4186CY093">>},
       {issue,<<>>},
       {st_pending_id,<<"PNDG">>},
       {st_submitted_id,<<"SBMT">>},
       {symbol,<<"ADBL">>},
       {trade_type_id,<<"TLB">>}
      ]
     },
    run(Channel, K, {K,V});
run(bh_2, Channel) ->
    K = 'ergen.BH.DM',
    V = [
         {acct_id,43000004637},
         {c_id,0},
         {co_id,0},
         {day_of_month,0},
         {vol_incr,0},
         {symbol,<<>>},
         {table_name,<<"ACCOUNT_PERMISSION">>},
         {tx_id,<<>>}
        ],
    run(Channel, K, {K,V});
run(bh_1, Channel) ->
    K = 'ergen.BH.CP',
    V = [
         {acct_id_idx,5},
         {cust_id,4300000047},
         {get_history,true},
         {tax_id,<<>>}
        ],
    run(Channel, K, {K,V});
run(mee, Channel) ->
    K = 'ergen.MEE.TM',
    V = [
         {price_quote,29.61},
         {trade_id,133},
         {trade_qty,800},
         {e_action,0},
         {symbol,<<"ADLAE">>},
         {trade_type_id,<<"TLB">>}
        ],
    run(Channel, K, {K,V}).

run(Channel, RoutingKey, Content) ->
    run(Channel, <<"ergen.topic">>, RoutingKey, Content).

run(Channel, Exchange, RoutingKey, Content)
  when is_pid(Channel), is_binary(Exchange), is_binary(RoutingKey), is_binary(Content) ->
    M = ergen_rpc_client,
    case M:start_link(Channel) of
        {ok, Pid} ->
            V = M:publish(Pid, Exchange, RoutingKey, Content),
            M:stop(Pid),
            V
    end;
run(Channel, Exchange, RoutingKey, Content)
  when is_atom(RoutingKey) ->
    run(Channel, Exchange, atom_to_binary(RoutingKey,latin1), Content);
run(Channel, Exchange, RoutingKey, Content) ->
    run(Channel, Exchange, RoutingKey, term_to_binary(Content)).

wait() ->
    receive timeout -> io:format("[wait] timeout!~n") end.

wait(Time) ->
    timer:send_after(Time, timeout),
    wait().

flush() ->
    receive M -> io:format("[flush] ~p~n", [M]), flush() after 0 -> ok end.
