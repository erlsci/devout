%%%-------------------------------------------------------------------
%%% @doc
%%% File system operations module - Implements all file system operations
%%% for the Devin clone MCP server
%%% @end
%%%-------------------------------------------------------------------
-module(devout_fs).

-export([
    % Core fs operations
    show_cwd/0,
    change_cwd/1,
    create_file/1,
    create_file/2,
    delete_file/1,
    create_directory/1,
    remove_directory/1,
    remove_directory/2,

    % MCP tool handlers
    handle_new_dir/1,
    handle_new_dirs/1,
    handle_move/1,
    handle_write/1,
    handle_read/1,
    handle_show_cwd/1,
    handle_change_cwd/1,
    handle_list_files/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Show current working directory
%% @end
%%--------------------------------------------------------------------
-spec show_cwd() -> {ok, string()} | {error, atom()}.
show_cwd() ->
    case file:get_cwd() of
        {ok, Cwd} ->
            ?LOG_INFO("Current working directory: ~s", [Cwd]),
            {ok, Cwd};
        Error ->
            ?LOG_ERROR("Failed to get current working directory: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Change current working directory to a relative path
%% @end
%%--------------------------------------------------------------------
-spec change_cwd(Path) -> {ok, string()} | {error, atom()} when
    Path :: string() | binary().
change_cwd(Path) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidatedPath} ->
            case file:set_cwd(ValidatedPath) of
                ok ->
                    {ok, NewCwd} = file:get_cwd(),
                    ?LOG_INFO("Changed working directory to: ~s", [NewCwd]),
                    {ok, NewCwd};
                Error ->
                    ?LOG_ERROR("Failed to change directory to ~s: ~p", [ValidatedPath, Error]),
                    Error
            end;
        Error ->
            ?LOG_ERROR("Invalid path for change_cwd: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Create a file with empty content
%% @end
%%--------------------------------------------------------------------
-spec create_file(Path) -> ok | {error, atom()} when
    Path :: string() | binary().
create_file(Path) ->
    create_file(Path, <<>>).

%%--------------------------------------------------------------------
%% @doc
%% Create a file with specified content
%% @end
%%--------------------------------------------------------------------
-spec create_file(Path, Content) -> ok | {error, atom()} when
    Path :: string() | binary(),
    Content :: binary() | string().
create_file(Path, Content) when is_list(Content) ->
    create_file(Path, list_to_binary(Content));
create_file(Path, Content) when is_binary(Content) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidatedPath} ->
            % Ensure parent directory exists
            ParentDir = filename:dirname(ValidatedPath),
            case ensure_directory_exists(ParentDir) of
                ok ->
                    case file:write_file(ValidatedPath, Content) of
                        ok ->
                            ?LOG_INFO("Created file: ~s", [ValidatedPath]),
                            ok;
                        Error ->
                            ?LOG_ERROR("Failed to create file ~s: ~p", [ValidatedPath, Error]),
                            Error
                    end;
                Error ->
                    ?LOG_ERROR("Failed to create parent directory for ~s: ~p", [ValidatedPath, Error]),
                    Error
            end;
        Error ->
            ?LOG_ERROR("Invalid path for create_file: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Delete a file
%% @end
%%--------------------------------------------------------------------
-spec delete_file(Path) -> ok | {error, atom()} when
    Path :: string() | binary().
delete_file(Path) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidatedPath} ->
            case file:delete(ValidatedPath) of
                ok ->
                    ?LOG_INFO("Deleted file: ~s", [ValidatedPath]),
                    ok;
                Error ->
                    ?LOG_ERROR("Failed to delete file ~s: ~p", [ValidatedPath, Error]),
                    Error
            end;
        Error ->
            ?LOG_ERROR("Invalid path for delete_file: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Create a directory and all necessary parent directories
%% @end
%%--------------------------------------------------------------------
-spec create_directory(Path) -> ok | {error, atom()} when
    Path :: string() | binary().
create_directory(Path) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidatedPath} ->
            case filelib:ensure_dir(ValidatedPath ++ "/") of
                ok ->
                    case file:make_dir(ValidatedPath) of
                        ok ->
                            ?LOG_INFO("Created directory: ~s", [ValidatedPath]),
                            ok;
                        {error, eexist} ->
                            ?LOG_INFO("Directory already exists: ~s", [ValidatedPath]),
                            ok;
                        Error ->
                            ?LOG_ERROR("Failed to create directory ~s: ~p", [ValidatedPath, Error]),
                            Error
                    end;
                Error ->
                    ?LOG_ERROR("Failed to ensure parent directories for ~s: ~p", [ValidatedPath, Error]),
                    Error
            end;
        Error ->
            ?LOG_ERROR("Invalid path for create_directory: ~p", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove a directory (only if empty)
%% @end
%%--------------------------------------------------------------------
-spec remove_directory(Path) -> ok | {error, atom()} when
    Path :: string() | binary().
remove_directory(Path) ->
    remove_directory(Path, false).

%%--------------------------------------------------------------------
%% @doc
%% Remove a directory, optionally removing all contents recursively
%% @end
%%--------------------------------------------------------------------
-spec remove_directory(Path, Recursive) -> ok | {error, atom() | {no_translation, binary()}} when
    Path :: string() | binary(),
    Recursive :: boolean().
remove_directory(Path, Recursive) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidatedPath} ->
            case Recursive of
                true ->
                    remove_directory_recursive(ValidatedPath);
                false ->
                    case file:del_dir(ValidatedPath) of
                        ok ->
                            ?LOG_INFO("Removed directory: ~s", [ValidatedPath]),
                            ok;
                        Error ->
                            ?LOG_ERROR("Failed to remove directory ~s: ~p", [ValidatedPath, Error]),
                            Error
                    end
            end;
        Error ->
            ?LOG_ERROR("Invalid path for remove_directory: ~p", [Error]),
            Error
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Ensure a directory exists, creating it if necessary
%% @end
%%--------------------------------------------------------------------
-spec ensure_directory_exists(Path) -> ok | {error, atom()} when
    Path :: string().
ensure_directory_exists(Path) ->
    case filelib:is_dir(Path) of
        true ->
            ok;
        false ->
            case filelib:ensure_dir(Path ++ "/") of
                ok ->
                    ok;
                Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove directory recursively
%% @end
%%--------------------------------------------------------------------
-spec remove_directory_recursive(Path) -> ok | {error, atom()} when
    Path :: string().
remove_directory_recursive(Path) ->
    case file:list_dir(Path) of
        {ok, Files} ->
            case remove_directory_contents(Path, Files) of
                ok ->
                    case file:del_dir(Path) of
                        ok ->
                            ?LOG_INFO("Removed directory recursively: ~s", [Path]),
                            ok;
                        Error ->
                            ?LOG_ERROR("Failed to remove directory after clearing contents ~s: ~p", [Path, Error]),
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            ?LOG_ERROR("Failed to list directory contents ~s: ~p", [Path, Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Remove all contents of a directory
%% @end
%%--------------------------------------------------------------------
-spec remove_directory_contents(Path, Files) -> ok | {error, atom()} when
    Path :: string(),
    Files :: [string()].
remove_directory_contents(_Path, []) ->
    ok;
remove_directory_contents(Path, [File | Rest]) ->
    FilePath = filename:join(Path, File),
    case filelib:is_dir(FilePath) of
        true ->
            case remove_directory_recursive(FilePath) of
                ok ->
                    remove_directory_contents(Path, Rest);
                Error ->
                    Error
            end;
        false ->
            case file:delete(FilePath) of
                ok ->
                    remove_directory_contents(Path, Rest);
                Error ->
                    ?LOG_ERROR("Failed to delete file ~s: ~p", [FilePath, Error]),
                    Error
            end
    end.

%%====================================================================
%% Tool handlers - Using functions above for actual operations
%%====================================================================

handle_new_dir(#{<<"path">> := Path}) ->
    case create_directory(Path) of
        ok ->
            <<"Directory created successfully: ", Path/binary>>;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
            <<"Error creating directory: ", ReasonBin/binary>>
    end.

handle_new_dirs(#{<<"path">> := Path} = Args) ->
    Children = maps:get(<<"children">>, Args, []),

    %% First create the base directory
    case create_directory(Path) of
        ok ->
            case create_children(Path, Children, []) of
                {ok, Results} ->
                    iolist_to_binary([<<"Directory structure created successfully:\n  - ">>,
                                      Path, Results]);
                {error, Reason} ->
                    ReasonBin = devout_fmt:error(Reason),
                    <<"Error creating child directories: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
            <<"Error creating base directory: ", ReasonBin/binary>>
    end.

create_children(_BasePath, [], Acc) ->
    {ok, lists:reverse(Acc)};
create_children(BasePath, [Child | Rest], Acc) ->
    ChildPath = <<BasePath/binary, "/", Child/binary>>,
    case create_directory(ChildPath) of
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
                    ReasonBin = devout_fmt:error(Reason),
                    <<"Error moving file: ", ReasonBin/binary>>
            end;
        {{error, SourceErr}, _} ->
            ReasonBin = devout_fmt:error(SourceErr),
            <<"Invalid source path: ", ReasonBin/binary>>;
        {_, {error, DestErr}} ->
            ReasonBin = devout_fmt:error(DestErr),
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
            case create_file(Path, Content) of
                ok when Mode =:= <<"append">> ->
                    % Handle append mode by reading existing content first
                    handle_append_write(Path, Content);
                ok ->
                    Size = byte_size(Content),
                    SizeBin = integer_to_binary(Size),
                    <<"Content written to file successfully: ", Path/binary,
                      " (", SizeBin/binary, " bytes)">>;
                {error, Reason} ->
                    ReasonBin = devout_fmt:error(Reason),
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
                    ReasonBin = devout_fmt:error(Reason),
                    <<"Error appending to file: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
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
                    ReasonBin = devout_fmt:error(Reason),
                    <<"Error reading file: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
            <<"Invalid path: ", ReasonBin/binary>>
    end.

handle_show_cwd(_Args) ->
    case show_cwd() of
        {ok, Cwd} ->
            CwdBin = list_to_binary(Cwd),
            <<"Current working directory: ", CwdBin/binary>>;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
            <<"Error getting current directory: ", ReasonBin/binary>>
    end.

handle_change_cwd(#{<<"path">> := Path}) ->
    case change_cwd(Path) of
        {ok, NewCwd} ->
            NewCwdBin = list_to_binary(NewCwd),
            <<"Changed working directory to: ", NewCwdBin/binary>>;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
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
                    ReasonBin = devout_fmt:error(Reason),
                    <<"Error listing directory: ", ReasonBin/binary>>
            end;
        {error, Reason} ->
            ReasonBin = devout_fmt:error(Reason),
            <<"Invalid path: ", ReasonBin/binary>>
    end.
