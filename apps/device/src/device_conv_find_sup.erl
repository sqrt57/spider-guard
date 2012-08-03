%% ===================================================================
%% Supervisor.
%% Supervises subsystem for finding converters.
%% ===================================================================

-module(device_conv_find_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10},
          [{conv_find,
              {device_conv_find, start_link, []},
              permanent,
              1000,
              worker,
              [device_conv_find]},
           {conv_find_workers_sup,
              {device_conv_find_workers_sup, start_link, []},
              permanent,
              1500,
              supervisor,
              [device_conv_find_workers_sup]}]}}.

