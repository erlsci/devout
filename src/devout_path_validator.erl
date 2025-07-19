%%%-------------------------------------------------------------------
%%% @doc
%%% Path validation module - Ensures only relative paths are allowed
%%% and paths don't escape the base directory
%%% @end
%%%-------------------------------------------------------------------
-module(devout_path_validator).

-export([
    validate_path/1,
    validate_path/2,
    resolve_path/1,
    resolve_path/2,
    is_relative_path/1,
    normalize_path/1
]).

-include_lib("kernel/include/logger.hrl").

-type path_error() :: absolute_path_not_allowed | parent_traversal_not_allowed | path_outside_base_directory.

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Validate a path is relative and safe
%% @end
%%--------------------------------------------------------------------
-spec validate_path(Path) -> {ok, string()} | {error, path_error()} when
    Path :: string() | binary().
validate_path(Path) ->
    {ok, BaseDir} = application:get_env(devout, base_directory),
    validate_path(Path, BaseDir).

%%--------------------------------------------------------------------
%% @doc
%% Validate a path is relative and safe with custom base directory
%% @end
%%--------------------------------------------------------------------
-spec validate_path(Path, BaseDir) -> {ok, string()} | {error, path_error()} when
    Path :: string() | binary(),
    BaseDir :: string().
validate_path(Path, BaseDir) when is_binary(Path) ->
    validate_path(binary_to_list(Path), BaseDir);
validate_path(Path, BaseDir) when is_list(Path) ->
    case is_relative_path(Path) of
        false ->
            ?LOG_WARNING("Rejected absolute path: ~s", [Path]),
            {error, absolute_path_not_allowed};
        true ->
            NormalizedPath = normalize_path(Path),
            case contains_parent_traversal(NormalizedPath) of
                true ->
                    ?LOG_WARNING("Rejected path with parent traversal: ~s", [Path]),
                    {error, parent_traversal_not_allowed};
                false ->
                    ResolvedPath = filename:join(BaseDir, NormalizedPath),
                    case is_within_base_directory(ResolvedPath, BaseDir) of
                        true ->
                            {ok, ResolvedPath};
                        false ->
                            ?LOG_WARNING("Rejected path outside base directory: ~s", [Path]),
                            {error, path_outside_base_directory}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc
%% Resolve a path relative to the base directory
%% @end
%%--------------------------------------------------------------------
-spec resolve_path(Path) -> {ok, string()} | {error, path_error()} when
    Path :: string() | binary().
resolve_path(Path) ->
    {ok, BaseDir} = application:get_env(devout, base_directory),
    resolve_path(Path, BaseDir).

%%--------------------------------------------------------------------
%% @doc
%% Resolve a path relative to a custom base directory
%% @end
%%--------------------------------------------------------------------
-spec resolve_path(Path, BaseDir) -> {ok, string()} | {error, path_error()} when
    Path :: string() | binary(),
    BaseDir :: string().
resolve_path(Path, BaseDir) ->
    case validate_path(Path, BaseDir) of
        {ok, ValidatedPath} ->
            {ok, ValidatedPath};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Check if a path is relative (not absolute)
%% @end
%%--------------------------------------------------------------------
-spec is_relative_path(Path) -> boolean() when
    Path :: string() | binary().
is_relative_path(Path) when is_binary(Path) ->
    is_relative_path(binary_to_list(Path));
is_relative_path(Path) when is_list(Path) ->
    case Path of
        [$/ | _] -> false;  % Unix absolute path
        [Drive, $: | _] when Drive >= $A, Drive =< $Z -> false;  % Windows absolute path
        [Drive, $: | _] when Drive >= $a, Drive =< $z -> false;  % Windows absolute path
        [$\\, $\\ | _] -> false;  % Windows UNC path
        _ -> true
    end.

%%--------------------------------------------------------------------
%% @doc
%% Normalize a path by removing redundant separators and dots
%% @end
%%--------------------------------------------------------------------
-spec normalize_path(Path) -> string() | binary() when
    Path :: string().
normalize_path(Path) ->
    % Split path into components
    Components = filename:split(Path),
    % Remove empty components and current directory references
    FilteredComponents = lists:filter(
        fun(Component) ->
            Component =/= "." andalso Component =/= ""
        end,
        Components
    ),
    % Join back together
    case FilteredComponents of
        [] -> ".";
        _ -> filename:join(FilteredComponents)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Check if a path contains parent directory traversal attempts
%% @end
%%--------------------------------------------------------------------
-spec contains_parent_traversal(Path) -> boolean() when
    Path :: string().
contains_parent_traversal(Path) ->
    Components = filename:split(Path),
    lists:member("..", Components).

%%--------------------------------------------------------------------
%% @doc
%% Check if a resolved path is within the base directory
%% @end
%%--------------------------------------------------------------------
-spec is_within_base_directory(ResolvedPath, BaseDir) -> boolean() when
    ResolvedPath :: string(),
    BaseDir :: string().
is_within_base_directory(ResolvedPath, BaseDir) ->
    % Get absolute paths for comparison
    AbsResolved = filename:absname(ResolvedPath),
    AbsBase = filename:absname(BaseDir),
    
    % Ensure both paths end with a separator for proper prefix checking
    NormalizedBase = case lists:last(AbsBase) of
        $/ -> AbsBase;
        _ -> AbsBase ++ "/"
    end,
    
    % Check if the resolved path starts with the base directory
    case string:prefix(AbsResolved ++ "/", NormalizedBase) of
        nomatch -> false;
        _ -> true
    end.