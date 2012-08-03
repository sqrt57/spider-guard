%% ===================================================================
%% Server.
%% Finds converters connected through LAN.
%% ===================================================================

-module(device_conv_find).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% Public API
-export([find_broadcast/0, find_broadcast/1,
         find_byaddress/1, find_byaddress/2]).

%% API for supervisor
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Includes and type definitions
-include("../include/info.hrl").

-type controller_version_rec() :: #controller_version{}.
-type converter_info_rec() :: #converter_info{}.
-type server_ref() :: Name :: atom() |
                      {Name :: atom(), Node :: atom()} |
                      {global, GlobalName :: term()} |
                      {via, Module :: atom(), ViaName :: term()} |
                      pid().

-export_type([controller_version_rec/0, converter_info_rec/0]).

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------

% Sends broadcast queries on all network interfaces,
% collects all replies from converters and returns them.
-spec find_broadcast() -> [converter_info_rec()].
find_broadcast() ->
    find_broadcast(?MODULE).

% Sends broadcast queries on all network interfaces,
% collects all replies from converters and returns them.
% The server which carries the search is specified.
-spec find_broadcast(ServerRef) -> [converter_info_rec()] when
    ServerRef :: server_ref().
find_broadcast(Server) ->
    gen_server:call(Server, {find, broadcast}, 3000).

% Sends query through network to a specific converter at Address
% and receives reply.
% Returns {ok, Result} when result is received.
% Returns timeout if no result is received during specific time interval.
% Returns cancelled if worker is killed during search.
-spec find_byaddress(Address) -> {ok, converter_info_rec()}
                               | timeout
                               | cancelled when
    Address :: inet:ip4_address().
find_byaddress(Address) ->
    find_byaddress(?MODULE, Address).

% Sends query through network to a specific converter at Address
% and receives reply.
% Returns {ok, Result} when result is received.
% Returns timeout if no result is received during specific time interval.
% Returns cancelled if worker is killed during search.
% The server which carries the search is specified.
-spec find_byaddress(ServerRef, Address) -> {ok, converter_info_rec()}
                                          | timeout
                                          | cancelled when
    ServerRef :: server_ref(),
    Address :: inet:ip4_address().
find_byaddress(Server, Address) ->
    gen_server:call(Server, {find, Address}, 3000).

%% ------------------------------------------------------------------
%% API for supervisor
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call({find, broadcast}, From, State) ->
    do_find_broadcast(From, State);
handle_call({find, Address}, From, State) ->
    do_find_byaddress(From, State, Address);
handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec do_find_broadcast(_Client, State) -> {noreply, State}.
do_find_broadcast(From, State) ->
    supervisor:start_child(device_conv_find_workers_sup, [From, broadcast]),
    {noreply, State}.
    
-spec do_find_byaddress(_Client, State, Address) -> {noreply, State} when
    Address :: inet:ip4_address().
do_find_byaddress(From, State, Address) ->
    supervisor:start_child(device_conv_find_workers_sup, [From, Address]),
    {noreply, State}.
