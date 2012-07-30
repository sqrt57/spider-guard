-module(device_conv_find).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% Public API
%% ------------------------------------------------------------------

-export([find_broadcast/0, find_broadcast/1,
         find_byaddress/1, find_byaddress/2]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Public API Function Definitions
%% ------------------------------------------------------------------

find_broadcast() ->
    find_broadcast(?MODULE).

find_broadcast(Server) ->
    gen_server:call(Server, {find, broadcast}, 3000).

find_byaddress(Address) ->
    find_byaddress(?MODULE, Address).

find_byaddress(Server, Address) ->
    gen_server:call(Server, {find, Address}, 3000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
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

do_find_broadcast(From, State) ->
    supervisor:start_child(device_conv_find_workers_sup, [From, broadcast]),
    {noreply, State}.
    
do_find_byaddress(From, State, Address) ->
    supervisor:start_child(device_conv_find_workers_sup, [From, Address]),
    {noreply, State}.
