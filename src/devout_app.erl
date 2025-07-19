%%%-------------------------------------------------------------------
%%% @doc
%%% Devout application module - Entry point for the Devout MCP server
%%% @end
%%%-------------------------------------------------------------------
-module(devout_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start_phase/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------
-spec start(StartType, StartArgs) -> {ok, pid()} | ignore | {error, Reason} when
    StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term(),
    Reason :: term().
start(_StartType, _StartArgs) ->
    ?LOG_INFO("Starting Devout MCP server application"),

    % Set the base directory to current working directory - FIXED VERSION
    case application:get_env(devout, base_directory) of
        {ok, undefined} ->
            % If explicitly set to undefined, override it
            {ok, Cwd} = file:get_cwd(),
            application:set_env(devout, base_directory, Cwd),
            ?LOG_INFO("Base directory fixed and set to: ~s", [Cwd]);
        undefined ->
            % If not set at all, set it
            {ok, Cwd} = file:get_cwd(),
            application:set_env(devout, base_directory, Cwd),
            ?LOG_INFO("Base directory set to: ~s", [Cwd]);
        {ok, ExistingDir} ->
            % Validate that the existing directory actually exists and is not undefined
            case ExistingDir of
                undefined ->
                    {ok, Cwd} = file:get_cwd(),
                    application:set_env(devout, base_directory, Cwd),
                    ?LOG_INFO("Base directory was undefined, fixed and set to: ~s", [Cwd]);
                _ ->
                    case filelib:is_dir(ExistingDir) of
                        true ->
                            ?LOG_INFO("Using existing base directory: ~s", [ExistingDir]);
                        false ->
                            ?LOG_WARNING("Base directory ~s does not exist, using current directory", [ExistingDir]),
                            {ok, Cwd} = file:get_cwd(),
                            application:set_env(devout, base_directory, Cwd),
                            ?LOG_INFO("Base directory set to: ~s", [Cwd])
                    end
            end
    end,

    case devout_sup:start_link() of
        {ok, Pid} ->
            ?LOG_INFO("Devout supervisor started successfully"),
            {ok, Pid};
        Error ->
            ?LOG_ERROR("Failed to start Devout supervisor: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called for each start phase defined in the
%% application specification, after the application and all its
%% dependencies have been started.
%% @end
%%--------------------------------------------------------------------
-spec start_phase(Phase, StartType, PhaseArgs) -> ok | {error, Reason} when
    Phase :: atom(),
    StartType :: normal | {takeover, node()} | {failover, node()},
    PhaseArgs :: term(),
    Reason :: term().
start_phase(register_tools, _StartType, _PhaseArgs) ->
    ?LOG_INFO("Starting phase: register_tools"),

    % First, start the erlmcp stdio server
    case erlmcp_sup:start_stdio_server() of
        {ok, Pid} ->
            ?LOG_INFO("erlmcp stdio server started successfully with pid: ~p", [Pid]),

            % Register our tools
            case register_tools() of
                ok ->
                    ?LOG_INFO("Successfully registered tools with erlmcp_stdio"),

                    % Monitor the stdio server and keep the application alive
                    spawn(fun() ->
                        monitor(process, Pid),
                        receive
                            {'DOWN', _, process, Pid, Reason} ->
                                ?LOG_INFO("stdio server terminated: ~p", [Reason]),
                                init:stop()
                        end
                    end),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register tools: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start erlmcp stdio server: ~p", [Reason]),
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Register all tools, resources, and prompts with erlmcp_stdio
%% @end
%%--------------------------------------------------------------------
-spec register_tools() -> ok | {error, {atom(), term()}}.
register_tools() ->
    ?LOG_INFO("Registering tools, resources, and prompts with erlmcp_stdio"),
    try
        % File system tools
        ok = devout_register:file_tools(),
        % Git tools
        ok = devout_register:git_tools(),
        % Resources and prompts
        ok = devout_register:resources(),
        ok = devout_register:prompts(),
        ok
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to register tools: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec stop(State) -> ok when
    State :: term().
stop(_State) ->
    ?LOG_INFO("Stopping Devout MCP server application"),
    ok.
