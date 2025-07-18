%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive test suite for devout MCP server
%%% Tests both the OTP structure and the file operations
%%% @end
%%%-------------------------------------------------------------------
-module(devout_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

-export([run/0]).

%%====================================================================
%% Test Suite
%%====================================================================

devout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Application and OTP tests", fun test_application_startup/0},
         {"Path validation tests", fun test_path_validation/0},
         {"File operations tests", fun test_file_operations/0},
         {"Directory operations tests", fun test_directory_operations/0},
         {"Security tests", fun test_security_features/0},
         {"Error handling tests", fun test_error_handling/0},
         {"Tool handler integration tests", fun test_tool_handlers/0}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Suppress info logs during tests
    logger:set_primary_config(level, error),
    
    % Create a temporary directory for testing
    TestDir = "/tmp/devout_test_" ++ integer_to_list(erlang:system_time()),
    file:make_dir(TestDir),
    
    % Start the application with test configuration
    application:set_env(devout, base_directory, TestDir),
    application:set_env(devout, max_file_size, 1024),  % 1KB for tests
    application:ensure_all_started(devout),
    
    TestDir.

cleanup(TestDir) ->
    % Stop the application
    application:stop(devout),
    
    % Clean up test directory recursively
    case file:list_dir(TestDir) of
        {ok, Files} ->
            cleanup_directory(TestDir, Files),
            file:del_dir(TestDir);
        _ ->
            ok
    end.

cleanup_directory(_Dir, []) ->
    ok;
cleanup_directory(Dir, [File | Rest]) ->
    FilePath = filename:join(Dir, File),
    case filelib:is_dir(FilePath) of
        true ->
            {ok, SubFiles} = file:list_dir(FilePath),
            cleanup_directory(FilePath, SubFiles),
            file:del_dir(FilePath);
        false ->
            file:delete(FilePath)
    end,
    cleanup_directory(Dir, Rest).

%%====================================================================
%% Test Functions
%%====================================================================

test_application_startup() ->
    % Test that the application is running
    ?assert(lists:member(devout, application:which_applications())),
    
    % Test that the main processes are running
    ?assert(is_pid(whereis(devout_sup))),
    ?assert(is_pid(whereis(devout_server))),
    
    % Test stdio server startup
    ?assertEqual(ok, devout_server:start_stdio()),
    ?assertEqual({error, already_started}, devout_server:start_stdio()).

test_path_validation() ->
    % Test relative path validation
    ?assertEqual({ok, _}, devout_path_validator:validate_path("test.txt")),
    ?assertEqual({ok, _}, devout_path_validator:validate_path("subdir/test.txt")),
    
    % Test absolute path rejection
    ?assertEqual({error, absolute_path_not_allowed}, 
                 devout_path_validator:validate_path("/absolute/path")),
    
    % Test parent traversal rejection
    ?assertEqual({error, parent_traversal_not_allowed}, 
                 devout_path_validator:validate_path("../escape")),
    ?assertEqual({error, parent_traversal_not_allowed}, 
                 devout_path_validator:validate_path("good/../bad")),
    
    % Test path normalization
    ?assertEqual("test/file.txt", devout_path_validator:normalize_path("test/./file.txt")),
    ?assertEqual("test/file.txt", devout_path_validator:normalize_path("test//file.txt")).

test_file_operations() ->
    TestDir = setup(),
    try
        % Test file creation
        FilePath = "test_file.txt",
        ?assertEqual(ok, devout_fs_ops:create_file(FilePath, <<"Hello, World!">>)),
        
        % Verify file exists and has correct content
        {ok, ValidPath} = devout_path_validator:validate_path(FilePath),
        ?assert(filelib:is_regular(ValidPath)),
        ?assertEqual({ok, <<"Hello, World!">>}, file:read_file(ValidPath)),
        
        % Test file deletion
        ?assertEqual(ok, devout_fs_ops:delete_file(FilePath)),
        ?assertNot(filelib:is_regular(ValidPath)),
        
        % Test creating file with directory structure
        NestedPath = "subdir/nested_file.txt",
        ?assertEqual(ok, devout_fs_ops:create_file(NestedPath, <<"Nested content">>)),
        {ok, ValidNested} = devout_path_validator:validate_path(NestedPath),
        ?assert(filelib:is_regular(ValidNested))
    after
        cleanup(TestDir)
    end.

test_directory_operations() ->
    TestDir = setup(),
    try
        % Test directory creation
        DirPath = "test_directory",
        ?assertEqual(ok, devout_fs_ops:create_directory(DirPath)),
        
        {ok, ValidDir} = devout_path_validator:validate_path(DirPath),
        ?assert(filelib:is_dir(ValidDir)),
        
        % Test nested directory creation
        NestedDir = "parent/child/grandchild",
        ?assertEqual(ok, devout_fs_ops:create_directory(NestedDir)),
        {ok, ValidNested} = devout_path_validator:validate_path(NestedDir),
        ?assert(filelib:is_dir(ValidNested)),
        
        % Test directory removal (non-recursive)
        EmptyDir = "empty_dir",
        ?assertEqual(ok, devout_fs_ops:create_directory(EmptyDir)),
        ?assertEqual(ok, devout_fs_ops:remove_directory(EmptyDir, false)),
        {ok, ValidEmpty} = devout_path_validator:validate_path(EmptyDir),
        ?assertNot(filelib:is_dir(ValidEmpty))
    after
        cleanup(TestDir)
    end.

test_security_features() ->
    % Test file size limits
    LargeContent = binary:copy(<<"x">>, 2048),  % Exceeds 1KB test limit
    FilePath = "large_file.txt",
    
    % This should fail due to size limit in the server layer
    % (we'll test the validation in the tool handler)
    Result = test_write_tool_handler(FilePath, LargeContent),
    ?assert(binary:match(Result, <<"exceeds maximum">>) =/= nomatch),
    
    % Test working directory constraints
    ?assertEqual({error, absolute_path_not_allowed}, 
                 devout_path_validator:validate_path("/etc/passwd")),
    ?assertEqual({error, parent_traversal_not_allowed}, 
                 devout_path_validator:validate_path("../../../etc/passwd")).

test_error_handling() ->
    % Test reading non-existent file
    ?assertEqual({error, enoent}, devout_fs_ops:delete_file("nonexistent.txt")),
    
    % Test deleting non-existent directory
    ?assertEqual({error, enoent}, devout_fs_ops:remove_directory("nonexistent", false)),
    
    % Test invalid paths
    ?assertEqual({error, absolute_path_not_allowed}, 
                 devout_path_validator:validate_path("/invalid/absolute/path")),
    ?assertEqual({error, parent_traversal_not_allowed}, 
                 devout_path_validator:validate_path("../invalid/traversal")).

test_tool_handlers() ->
    TestDir = setup(),
    try
        % Test tool handlers directly (these are the functions called by erlmcp)
        
        % Test new-dir handler
        NewDirResult = test_new_dir_handler("test_tool_dir"),
        ?assert(binary:match(NewDirResult, <<"created successfully">>) =/= nomatch),
        
        % Test new-dirs handler  
        NewDirsResult = test_new_dirs_handler("project", [<<"src">>, <<"test">>]),
        ?assert(binary:match(NewDirsResult, <<"structure created">>) =/= nomatch),
        
        % Test write handler
        WriteResult = test_write_tool_handler("test.txt", <<"content">>),
        ?assert(binary:match(WriteResult, <<"written to file successfully">>) =/= nomatch),
        
        % Test read handler
        ReadResult = test_read_handler("test.txt"),
        ?assert(binary:match(ReadResult, <<"Content of">>) =/= nomatch),
        ?assert(binary:match(ReadResult, <<"content">>) =/= nomatch),
        
        % Test move handler
        MoveResult = test_move_handler("test.txt", "moved_test.txt"),
        ?assert(binary:match(MoveResult, <<"Moved successfully">>) =/= nomatch),
        
        % Test show-cwd handler
        CwdResult = test_show_cwd_handler(),
        ?assert(binary:match(CwdResult, <<"Current working directory">>) =/= nomatch)
    after
        cleanup(TestDir)
    end.

%%====================================================================
%% Helper Functions - Tool Handler Testing
%%====================================================================

test_new_dir_handler(Path) ->
    devout_server:handle_new_dir(#{<<"path">> => list_to_binary(Path)}).

test_new_dirs_handler(Path, Children) ->
    ChildrenBin = [list_to_binary(C) || C <- Children],
    devout_server:handle_new_dirs(#{<<"path">> => list_to_binary(Path), 
                                    <<"children">> => ChildrenBin}).

test_write_tool_handler(Path, Content) ->
    devout_server:handle_write(#{<<"path">> => list_to_binary(Path), 
                                 <<"content">> => Content}).

test_read_handler(Path) ->
    devout_server:handle_read(#{<<"path">> => list_to_binary(Path)}).

test_move_handler(Source, Dest) ->
    devout_server:handle_move(#{<<"source">> => list_to_binary(Source),
                                <<"destination">> => list_to_binary(Dest)}).

test_show_cwd_handler() ->
    devout_server:handle_show_cwd(#{}).

%%====================================================================
%% Test Runner
%%====================================================================

run() ->
    eunit:test(?MODULE, [verbose]).