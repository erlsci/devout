%%%-------------------------------------------------------------------
%%% @doc
%%% File system operations module - Implements all file system operations
%%% for the Devin clone MCP server
%%% @end
%%%-------------------------------------------------------------------
-module(devout_fs_ops).

-export([
    show_cwd/0,
    change_cwd/1,
    create_file/1,
    create_file/2,
    delete_file/1,
    create_directory/1,
    remove_directory/1,
    remove_directory/2
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
create_file(Path, Content) ->
    case devout_path_validator:validate_path(Path) of
        {ok, ValidatedPath} ->
            % Ensure parent directory exists
            ParentDir = filename:dirname(ValidatedPath),
            case ensure_directory_exists(ParentDir) of
                ok ->
                    BinaryContent = ensure_binary(Content),
                    case file:write_file(ValidatedPath, BinaryContent) of
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
%% Ensure content is binary
%% @end
%%--------------------------------------------------------------------
-spec ensure_binary(Content) -> binary() when
    Content :: binary() | string().
ensure_binary(Content) when is_binary(Content) ->
    Content;
ensure_binary(Content) when is_list(Content) ->
    list_to_binary(Content).

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