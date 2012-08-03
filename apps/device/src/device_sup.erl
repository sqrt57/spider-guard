%% ===================================================================
%% Top supervisor in device application.
%% ===================================================================

-module(device_sup).

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
          [{conv_find_sup,
              {device_conv_find_sup, start_link, []},
              permanent,
              2000,
              supervisor,
              [device_conv_find_sup]}]}}.

