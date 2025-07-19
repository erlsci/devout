%%%-------------------------------------------------------------------
%%% @doc
%%% Devout top level supervisor - Manages the MCP server process
%%% @end
%%%-------------------------------------------------------------------
-module(devout_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    ?LOG_INFO("Starting Devout supervisor"),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} when
    Args :: term(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().
init([]) ->
    ?LOG_INFO("Initializing Devout supervisor"),

    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = #{
        strategy => one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
    },

    % Child specification for the MCP server
    DevoutServer = #{
        id => devout_server,
        start => {devout_server, start_link, []},
        restart => permanent,
        shutdown => brutal_kill,
        type => worker,
        modules => [devout_server]
    },

    Children = [DevoutServer],

    ?LOG_INFO("Devout supervisor initialized with ~p children", [length(Children)]),
    {ok, {SupFlags, Children}}.
