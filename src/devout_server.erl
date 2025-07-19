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

%% Tool handlers
-export([handle_new_dir/1, handle_new_dirs/1, handle_move/1, handle_write/1,
         handle_read/1, handle_show_cwd/1, handle_change_cwd/1, handle_list_files/1]).

%% Resource handlers
-export([handle_status_resource/1, handle_help_resource/1]).

%% Prompt handlers
-export([handle_create_project_prompt/1]).

%% Utility functions
-export([format_error/1]).

-include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {
    base_directory :: string()
}).

-type state() :: #state{}.

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

    State = #state{base_directory = BaseDir},

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
%% Tool handlers - Using devout_fs for actual operations
%%====================================================================

handle_new_dir(#{<<"path">> := Path}) ->
    case devout_fs:create_directory(Path) of
        ok ->
            <<"Directory created successfully: ", Path/binary>>;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error creating directory: ", ReasonBin/binary>>
    end.

handle_new_dirs(#{<<"path">> := Path} = Args) ->
    Children = maps:get(<<"children">>, Args, []),

    %% First create the base directory
    case devout_fs:create_directory(Path) of
        ok ->
            case create_children(Path, Children, []) of
                {ok, Results} ->
                    iolist_to_binary([<<"Directory structure created successfully:\n  - ">>,
                                      Path, Results]);
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error creating child directories: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error creating base directory: ", ReasonBin/binary>>
    end.

create_children(_BasePath, [], Acc) ->
    {ok, lists:reverse(Acc)};
create_children(BasePath, [Child | Rest], Acc) ->
    ChildPath = <<BasePath/binary, "/", Child/binary>>,
    case devout_fs:create_directory(ChildPath) of
        ok ->
            create_children(BasePath, Rest, [<<"\n  - ", Child/binary>> | Acc]);
        {error, Reason} ->
            {error, {child_creation_failed, Child, Reason}}
    end.

handle_move(#{<<"source">> := Source, <<"destination">> := Destination}) ->
    %% Use path validator to ensure both paths are safe
    case {devout_path_validator:validate_path(Source),
          devout_path_validator:validate_path(Destination)} of
        {{ok, ValidSource}, {ok, ValidDest}} ->
            case file:rename(ValidSource, ValidDest) of
                ok ->
                    <<"Moved successfully: ", Source/binary, " -> ", Destination/binary>>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error moving file: ", ReasonBin/binary>>
            end;
        {{error, SourceErr}, _} ->
            ReasonBin = format_error(SourceErr),
            <<"Invalid source path: ", ReasonBin/binary>>;
        {_, {error, DestErr}} ->
            ReasonBin = format_error(DestErr),
            <<"Invalid destination path: ", ReasonBin/binary>>
    end.

handle_write(#{<<"path">> := Path, <<"content">> := Content} = Args) ->
    Mode = maps:get(<<"mode">>, Args, <<"write">>),

    %% Check file size limit
    {ok, MaxSize} = application:get_env(devout, max_file_size),
    case byte_size(Content) > MaxSize of
        true ->
            MaxSizeBin = integer_to_binary(MaxSize),
            <<"Error: Content exceeds maximum file size (", MaxSizeBin/binary, " bytes)">>;
        false ->
            case devout_fs:create_file(Path, Content) of
                ok when Mode =:= <<"append">> ->
                    % Handle append mode by reading existing content first
                    handle_append_write(Path, Content);
                ok ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    <<"Content written to file successfully: ", Path/binary,
                      " (", SizeBin/binary, " bytes)">>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error writing file: ", ReasonBin/binary>>
            end
    end.

handle_append_write(Path, Content) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidPath} ->
            case file:write_file(ValidPath, Content, [append]) of
                ok ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    <<"Content appended to file successfully: ", Path/binary,
                      " (", SizeBin/binary, " bytes)">>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error appending to file: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Invalid path for append: ", ReasonBin/binary>>
    end.

handle_read(#{<<"path">> := Path}) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidPath} ->
            case file:read_file(ValidPath) of
                {ok, Content} ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    case Content of
                        <<>> ->
                            <<"File is empty: ", Path/binary>>;
                        _ ->
                            <<"Content of ", Path/binary, " (", SizeBin/binary, " bytes):\n\n", Content/binary>>
                    end;
                {error, enoent} ->
                    <<"Error: File does not exist: ", Path/binary>>;
                {error, eisdir} ->
                    <<"Error: Path is a directory, not a file: ", Path/binary>>;
                {error, eacces} ->
                    <<"Error: Permission denied reading file: ", Path/binary>>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error reading file: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Invalid path: ", ReasonBin/binary>>
    end.

handle_show_cwd(_Args) ->
    case devout_fs:show_cwd() of
        {ok, Cwd} ->
            CwdBin = list_to_binary(Cwd),
            <<"Current working directory: ", CwdBin/binary>>;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error getting current directory: ", ReasonBin/binary>>
    end.

handle_change_cwd(#{<<"path">> := Path}) ->
    case devout_fs:change_cwd(Path) of
        {ok, NewCwd} ->
            NewCwdBin = list_to_binary(NewCwd),
            <<"Changed working directory to: ", NewCwdBin/binary>>;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Error changing directory: ", ReasonBin/binary>>
    end.

handle_list_files(Args) ->
    Path = maps:get(<<"path">>, Args, <<".">>),

    % Handle current directory specially
    ValidPath = case Path of
        <<".">> ->
            case file:get_cwd() of
                {ok, Cwd} -> {ok, Cwd};
                Error -> Error
            end;
        _ ->
            devout_path_validator:validate_path(Path)
    end,

    case ValidPath of
        {ok, DirPath} ->
            case file:list_dir(DirPath) of
                {ok, Files} ->
                    case Files of
                        [] ->
                            <<"Directory is empty: ", Path/binary>>;
                        _ ->
                            % Sort files and get detailed info
                            DetailedFiles = lists:map(fun(File) ->
                                FilePath = filename:join(DirPath, File),
                                case file:read_file_info(FilePath) of
                                    {ok, FileInfo} ->
                                        Type = case FileInfo#file_info.type of
                                            directory -> <<"[DIR]">>;
                                            regular -> <<"[FILE]">>;
                                            symlink -> <<"[LINK]">>;
                                            _ -> <<"[OTHER]">>
                                        end,
                                        Size = case FileInfo#file_info.type of
                                            directory -> <<"">>;
                                            _ ->
                                                SizeBytes = FileInfo#file_info.size,
                                                <<" (", (integer_to_binary(SizeBytes))/binary, " bytes)">>
                                        end,
                                        FileBin = list_to_binary(File),
                                        <<Type/binary, " ", FileBin/binary, Size/binary>>;
                                    {error, _} ->
                                        FileBin = list_to_binary(File),
                                        <<"[?] ", FileBin/binary, " (info unavailable)">>
                                end
                            end, lists:sort(Files)),

                            FilesList = lists:join(<<"\n">>, DetailedFiles),
                            iolist_to_binary([<<"Contents of ", Path/binary, ":\n">>, FilesList])
                    end;
                {error, enoent} ->
                    <<"Error: Directory does not exist: ", Path/binary>>;
                {error, enotdir} ->
                    <<"Error: Path is not a directory: ", Path/binary>>;
                {error, eacces} ->
                    <<"Error: Permission denied accessing directory: ", Path/binary>>;
                {error, Reason} ->
                    ReasonBin = format_error(Reason),
                    <<"Error listing directory: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = format_error(Reason),
            <<"Invalid path: ", ReasonBin/binary>>
    end.

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

%%====================================================================
%% Utility functions
%%====================================================================

format_error({child_creation_failed, Child, SubReason}) ->
    ChildBin = ensure_binary(Child),
    SubReasonBin = format_error(SubReason),
    <<"Failed to create child directory ", ChildBin/binary, ": ", SubReasonBin/binary>>;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8).

ensure_binary(B) when is_binary(B) -> B.
