%%%-------------------------------------------------------------------
%%% @doc
%%% Devout MCP server - Tool and resource handlers for erlmcp
%%% @end
%%%-------------------------------------------------------------------
-module(devout_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Resource and prompt handlers
-export([
    handle_status_resource/1, 
    handle_help_resource/1,
    handle_create_project_prompt/1
]).

-include("devout.hrl").
-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-type state() :: #devout_server_state{}.

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Initializing Devout MCP server"),

    % Get base directory
    {ok, BaseDir} = application:get_env(devout, base_directory),

    % Note: We don't register tools here anymore - that happens in the start_phase

    State = #devout_server_state{base_directory = BaseDir},

    ?LOG_INFO("Devout MCP server initialized with base directory: ~s", [BaseDir]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("Devout MCP server terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Resource handlers
%%====================================================================

handle_status_resource(_Uri) ->
    {ok, Cwd} = file:get_cwd(),
    CwdBin = list_to_binary(Cwd),
    {ok, AllowedOps} = application:get_env(devout, allowed_operations),
    OpsList = string:join([atom_to_list(Op) || Op <- AllowedOps], ", "),
    OpsListBin = list_to_binary(OpsList),

    <<"Devout MCP Service Status\n\n"
      "Current working directory: ", CwdBin/binary, "\n"
      "Service: Active\n"
      "Allowed operations: ", OpsListBin/binary, "\n"
      "Security: Path validation enabled\n">>.

handle_help_resource(_Uri) ->
    <<"Devout MCP Service Help\n\n"
      "Available Tools:\n"
      "- new-dir: Create a new directory\n"
      "  Usage: {\"path\": \"relative/path\"}\n\n"
      "- new-dirs: Create a directory with children\n"
      "  Usage: {\"path\": \"base/path\", \"children\": [\"dir1\", \"dir2\"]}\n\n"
      "- move: Move or rename a file or directory\n"
      "  Usage: {\"source\": \"old/path\", \"destination\": \"new/path\"}\n\n"
      "- write: Create or write to a file\n"
      "  Usage: {\"path\": \"file.txt\", \"content\": \"data\", \"mode\": \"write|append\"}\n\n"
      "- read: Read a file\n"
      "  Usage: {\"path\": \"file.txt\"}\n\n"
      "- show-cwd: Show current working directory\n"
      "  Usage: {}\n\n"
      "- change-cwd: Change current working directory\n"
      "  Usage: {\"path\": \"relative/path\"}\n\n"
      "Security Features:\n"
      "- All paths must be relative\n"
      "- Path traversal attacks prevented\n"
      "- Operations restricted to base directory\n"
      "- File size limits enforced\n">>.

%%====================================================================
%% Prompt handlers
%%====================================================================

handle_create_project_prompt(Args) ->
    ProjectName = maps:get(<<"project_name">>, Args, <<"my_project">>),
    ProjectType = maps:get(<<"project_type">>, Args, <<"web">>),

    {Instructions, Dirs} = case ProjectType of
        <<"erlang">> ->
            {<<"Create an Erlang/OTP project structure">>,
             [<<"src">>, <<"include">>, <<"test">>, <<"priv">>, <<"rebar.config">>]};
        <<"web">> ->
            {<<"Create a web project structure">>,
             [<<"src">>, <<"static">>, <<"templates">>, <<"config">>, <<"tests">>]};
        <<"api">> ->
            {<<"Create an API project structure">>,
             [<<"src">>, <<"tests">>, <<"docs">>, <<"config">>, <<"schemas">>]};
        <<"library">> ->
            {<<"Create a library project structure">>,
             [<<"src">>, <<"tests">>, <<"docs">>, <<"examples">>, <<"benchmarks">>]};
        _ ->
            {<<"Create a basic project structure">>,
             [<<"src">>, <<"tests">>, <<"docs">>]}
    end,

    DirsText = string:join([binary_to_list(D) || D <- Dirs], ", "),
    DirsTextBin = list_to_binary(DirsText),

    [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"Create a project structure for '", ProjectName/binary,
                            "' (", ProjectType/binary, " project). ", Instructions/binary,
                            " with directories: ", DirsTextBin/binary,
                            ". Use the devout tools to create the necessary directories and files.">>
        }
    }].