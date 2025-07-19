%%%-------------------------------------------------------------------
%%% @doc
%%% Simple unit tests for devout components without full application startup
%%% @end
%%%-------------------------------------------------------------------
-module(devout_simple_test).

-include("devout.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Path Validator Tests
%%====================================================================

path_validator_test_() ->
    [
        {"Relative paths", fun test_relative_paths/0},
        {"Absolute paths", fun test_absolute_paths/0},
        {"Parent traversal", fun test_parent_traversal/0},
        {"Path normalization", fun test_path_normalization/0}
    ].

test_relative_paths() ->
    % These should all be valid relative paths
    ?assert(devout_path_validator:is_relative_path("test.txt")),
    ?assert(devout_path_validator:is_relative_path("dir/test.txt")),
    ?assert(devout_path_validator:is_relative_path("./test.txt")),
    ?assert(devout_path_validator:is_relative_path("dir/subdir/test.txt")).

test_absolute_paths() ->
    % These should all be detected as absolute paths
    ?assertNot(devout_path_validator:is_relative_path("/test.txt")),
    ?assertNot(devout_path_validator:is_relative_path("/home/user/test.txt")),
    % Windows paths
    ?assertNot(devout_path_validator:is_relative_path("C:/test.txt")),
    ?assertNot(devout_path_validator:is_relative_path("c:\\test.txt")),
    ?assertNot(devout_path_validator:is_relative_path("\\\\server\\share")).

test_parent_traversal() ->
    % With a base directory set, these should be rejected
    BaseDir = "/tmp/test_base",
    ?assertEqual({error, parent_traversal_not_allowed},
                 devout_path_validator:validate_path("../escape", BaseDir)),
    ?assertEqual({error, parent_traversal_not_allowed},
                 devout_path_validator:validate_path("good/../../../bad", BaseDir)),
    ?assertEqual({error, parent_traversal_not_allowed},
                 devout_path_validator:validate_path("./good/../../bad", BaseDir)).

test_path_normalization() ->
    % Test that paths are normalized correctly
    ?assertEqual("test.txt", devout_path_validator:normalize_path("test.txt")),
    ?assertEqual("test.txt", devout_path_validator:normalize_path("./test.txt")),
    ?assertEqual("dir/test.txt", devout_path_validator:normalize_path("dir/test.txt")),
    ?assertEqual("dir/test.txt", devout_path_validator:normalize_path("dir/./test.txt")),
    ?assertEqual("dir/test.txt", devout_path_validator:normalize_path("dir//test.txt")),
    ?assertEqual(".", devout_path_validator:normalize_path(".")),
    ?assertEqual(".", devout_path_validator:normalize_path("./.")).

%%====================================================================
%% File System Operations Tests (without actual file I/O)
%%====================================================================

fs_ops_error_handling_test_() ->
    [
        {"Binary path handling", fun test_binary_paths/0}
    ].

test_binary_paths() ->
    % Test that binary paths are handled correctly
    BaseDir = "/tmp/test",
    ?assertMatch({ok, _}, devout_path_validator:validate_path(<<"test.txt">>, BaseDir)),
    ?assertEqual({error, absolute_path_not_allowed},
                 devout_path_validator:validate_path(<<"/etc/passwd">>, BaseDir)).

%%====================================================================
%% Tool Handler Message Format Tests
%%====================================================================

message_format_test_() ->
    [
        {"Error formatting", fun test_error_formatting/0}
    ].

test_error_formatting() ->
    % Test error message formatting
    Result = devout_fmt:err(enoent),
    ?assertEqual(<<"enoent">>, Result),

    Result2 = devout_fmt:err({child_creation_failed, <<"test">>, eacces}),
    ?assert(binary:match(Result2, <<"Failed to create child directory">>) =/= nomatch).