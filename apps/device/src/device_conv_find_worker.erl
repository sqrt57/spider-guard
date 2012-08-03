%% ===================================================================
%% Temporary worker.
%% Performs one operation and exits.
%% Finds converter/converters connected through LAN.
%% ===================================================================

-module(device_conv_find_worker).

%% API for supervisor
-export([start_link/2]).

%% Internal API
-export([do_find_broadcast/1, do_find_byaddress/2]).

%% Includes
-include("../include/info.hrl").

%% Constants
-define(FIND_REQUEST, [16#52, 16#47]).
-define(BYADDRESS_TIMEOUT, 1000).
-define(BROADCAST_TIMEOUT, 1000).
-define(CONTROLLER_PORT, 5001).

%% ------------------------------------------------------------------
%% API for supervisor
%% ------------------------------------------------------------------

-spec start_link(_Client, Address) -> {ok, pid()} | {error, _Reason} when
    Address :: inet:ip4_address() | broadcast.
start_link(From, broadcast) ->
    proc_lib:start_link(?MODULE, do_find_broadcast, [From]);
start_link(From, Address) ->
    proc_lib:start_link(?MODULE, do_find_byaddress, [From, Address]).

%% ------------------------------------------------------------------
%% Internal API
%% ------------------------------------------------------------------

-spec do_find_broadcast(_Client) -> any().
do_find_broadcast(From) ->
    Addresses = get_addr(),
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

-spec get_addr() -> [inet:ip4_address()].
get_addr() ->
    {ok, Ifs} = inet:getifaddrs(),
    lists:reverse(lists:foldl(fun add_broadcast/2, [], Ifs)).

-spec open_broadcast(inet:ip_address()) -> {ok, Socket} | {error, Reason} when
    Socket :: inet:socket(),
    Reason :: inet:posix().
open_broadcast(Ip) ->
    gen_udp:open(0,
                 [binary,
                  {ip, Ip},
                  {active, true},
                  {broadcast, true}]).

-spec add_broadcast({Ifname, Params}, Acc) -> Acc when
    Acc :: [inet_ip4_address],
    Ifname :: string(),
    Params :: {flag, Flag}
            | {addr, inet:ip_address()}
            | {netmask, inet:ip_address()}
            | {broadaddr, inet:ip_address()}
            | {dstaddr, inet:ip_address()}
            | {hwaddr, [byte()]},
    Flag :: up | broadcast | loopback | pointtopoint | running | multicast.
add_broadcast({_Ifname, Params}, Acc) ->
    {flags, Flags} = lists:keyfind(flags, 1, Params),
    case lists:member(broadcast, Flags) of
        true -> case lists:filter(fun is_ip4_tuple/1, Params) of
                [{addr, Address}|_] -> [ Address | Acc ];
                [] -> acc
            end;
        false -> Acc
    end.

-spec is_ip4_tuple(any()) -> boolean().
is_ip4_tuple({addr, {_, _, _, _}}) -> true;
is_ip4_tuple(_) -> false.

-spec find_broadcast_loop(_Client, Sockets, Acc) -> any() when
    Sockets :: [inet:socket()],
    Acc :: [#converter_info{}].
find_broadcast_loop(From, Sockets, Acc) ->
    receive
        {udp, _Socket, Address, ?CONTROLLER_PORT, Data} ->
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
    
-spec do_find_byaddress(_Client, Address) -> any() when
    Address :: inet:ip4_address().
do_find_byaddress(From, Address) ->
    {ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
    proc_lib:init_ack({ok, self()}),
    process_flag(trap_exit, true),
    gen_udp:send(Socket, Address, 5001, ?FIND_REQUEST),
    timer:send_after(?BYADDRESS_TIMEOUT, done),
    find_byaddress_loop(From, Socket, Address).

-spec find_byaddress_loop(_Client, Socket, Address) -> any() when
    Socket :: inet:socket(),
    Address :: inet:ip4_address().
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

-spec send_result(_From, _Result) -> any().
send_result(From, Result) ->
    gen_server:reply(From, Result).

-spec try_parse_data(Address, Data) -> {ok, Info} | false when
    Address :: inet:ip4_address(),
    Data :: binary(),
    Info :: #converter_info{}.
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

-spec bin_to_ip(binary()) -> tuple().
bin_to_ip(Bin) -> list_to_tuple(binary_to_list(Bin)).