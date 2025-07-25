%% @doc Common definitions and records for devout application
%%
%% This header file contains shared types, records, and macros
%% used across the devout application modules.

-ifndef(DEVOUT_HRL).
-define(DEVOUT_HRL, 1).

%% Application information
-define(APP_NAME, devout).
-define(APP_VERSION, "0.1.0").

%% Default configuration values
-define(DEFAULT_MAX_FILE_SIZE, 10485760). % 10MB
-define(DEFAULT_GIT_TIMEOUT, 30000).      % 30 seconds

%% Git command results
-type git_result() :: {ok, binary()} | {error, {exit_code, integer(), binary()}} | {error, term()}.

%% File operation results
-type file_result() :: {ok, binary()} | {error, atom() | {atom(), term()}}.

%% Path validation errors
-type path_error() :: absolute_path_not_allowed | parent_traversal_not_allowed | path_outside_base_directory.

%% Configuration record
-record(devout_config, {
    max_file_size = ?DEFAULT_MAX_FILE_SIZE :: pos_integer(),
    allowed_extensions = all :: all | [binary()],
    allowed_operations = [] :: [atom()],
    git_timeout = ?DEFAULT_GIT_TIMEOUT :: pos_integer()
}).

%% Server state record
-record(devout_server_state, {
    base_directory :: string()
}).

%% Export types for other modules to use
-type devout_config() :: #devout_config{}.
-type devout_server_state() :: #devout_server_state{}.

-export_type([
    git_result/0, 
    file_result/0, 
    path_error/0,
    devout_config/0,
    devout_server_state/0
]).

-endif. % DEVOUT_HRL