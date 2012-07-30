
-module(device_conv_find_worker).

-export([start_link/2]).

-export([do_find_broadcast/1, do_find_byaddress/2]).

-include("../include/info.hrl").
-define(FIND_REQUEST, [16#52, 16#47]).
-define(BYADDRESS_TIMEOUT, 1000).
-define(BROADCAST_TIMEOUT, 1000).
-define(CONTROLLER_PORT, 5001).

start_link(From, broadcast) ->
    proc_lib:start_link(?MODULE, do_find_broadcast, [From]);
start_link(From, Address) ->
    proc_lib:start_link(?MODULE, do_find_byaddress, [From, Address]).

do_find_broadcast(From) ->
    {ok, Ifs} = inet:getifaddrs(),
    Addresses = get_addr(Ifs),
    Results = lists:map(fun open_broadcast/1, Addresses),
    Ok_results = lists:filter(fun({ok, _}) -> true; (_) -> false end, Results),
    Sockets = lists:map(fun({ok, Sock}) -> Sock end, Ok_results),
    proc_lib:init_ack({ok, self()}),
    process_flag(trap_exit, true),
    lists:map(fun(S) ->
                  gen_udp:send(S, {255,255,255,255}, 5001, ?FIND_REQUEST) end,
              Sockets),
    timer:send_after(?BROADCAST_TIMEOUT, done),
    find_broadcast_loop(From, Sockets, []).

get_addr(Ifaddrs) ->
    lists:reverse(lists:foldl(fun add_broadcast/2, [], Ifaddrs)).

open_broadcast(Ip) ->
    gen_udp:open(0,
                 [binary,
                  {ip, Ip},
                  {active, true},
                  {broadcast, true}]).
  
add_broadcast({_Name, Params}, Acc) ->
    {flags, Flags} = lists:keyfind(flags, 1, Params),
    case lists:member(broadcast, Flags) of
        true -> case lists:filter(fun is_ip4_tuple/1, Params) of
                [{addr, Address}|_] -> [ Address | Acc ];
                [] -> acc
            end;
        false -> Acc
    end.

is_ip4_tuple({addr, {_, _, _, _}}) -> true;
is_ip4_tuple(_) -> false.

find_broadcast_loop(From, Sockets, Acc) ->
    receive
        {udp, Socket, Address, ?CONTROLLER_PORT, Data} ->
            case try_parse_data(Address, Data) of
                {ok, Result} ->
                    find_broadcast_loop(From, Sockets, [ Result | Acc ]);
                _ -> find_broadcast_loop(From, Sockets, Address)
            end;
        done -> lists:map(fun gen_udp:close/1, Sockets),
                send_result(From, Acc);
        {'EXIT', _Pid, _Reason} -> lists:map(fun gen_udp:close/1, Sockets),
                                   send_result(From, Acc);
        _ -> find_broadcast_loop(From, Sockets, Acc)
    end.
    
do_find_byaddress(From, Address) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
    proc_lib:init_ack({ok, self()}),
    process_flag(trap_exit, true),
    gen_udp:send(Socket, Address, 5001, ?FIND_REQUEST),
    timer:send_after(?BYADDRESS_TIMEOUT, done),
    find_byaddress_loop(From, Socket, Address).

find_byaddress_loop(From, Socket, Address) ->
    receive
        {udp, Socket, Address, ?CONTROLLER_PORT, Data} ->
            case try_parse_data(Address, Data) of
                {ok, Result} -> gen_udp:close(Socket),
                                send_result(From, {ok, Result});
                _ -> find_byaddress_loop(From, Socket, Address)
            end;
        done -> gen_udp:close(Socket),
                send_result(From, timeout);
        {'EXIT', _Pid, _Reason} -> gen_udp:close(Socket),
                                   send_result(From, cancelled);
        _ -> find_byaddress_loop(From, Socket, Address)
    end.
    
send_result(From, Result) ->
    gen_server:reply(From, Result).
    
try_parse_data(Address, Data = <<Ip:4/bytes, Mask:4/bytes, Gateway:4/bytes,
                                 _ServerIp:4/bytes, Port:16/little, Mac:6/bytes,
                                 _Flags:7, Encryption:1,
                                 Board:8, Major:8, Minor:8,
                                 16#ff:8, _Sum:8>>) ->
    case lists:sum(binary:bin_to_list(Data)) band 16#ff of
        0 ->
            Version = #controller_version { board = Board,
                                            major = Major,
                                            minor = Minor },
            Info = #converter_info { ip = bin_to_ip(Ip),
                                     mask = bin_to_ip(Mask),
                                     gateway = bin_to_ip(Gateway),
                                     port = Port,
                                     mac = bin_to_ip(Mac),
                                     encryption = Encryption =/= 0,
                                     version = Version },
            case Info#converter_info.ip =:= Address of
                true -> {ok, Info};
                false -> false
            end;
        _ -> false
    end.

bin_to_ip(Bin) -> list_to_tuple(binary_to_list(Bin)).