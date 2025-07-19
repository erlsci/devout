%% @doc Tests for devout_git module
%%
%% This module contains unit tests for the Git operations functionality.
%% Tests cover both the core Git operations and MCP tool handlers.

-module(devout_git_test).

-include("devout.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test fixtures and setup

setup_git_test() ->
    % Setup for git tests - set test mode and start erlexec application
    application:set_env(devout, test_mode, true),
    application:set_env(devout, git_timeout, 2000),  % Very short timeout for tests
    case application:ensure_all_started(erlexec) of
        {ok, Started} -> 
            io:format("Started applications for git tests: ~p~n", [Started]),
            ok;
        {error, {erlexec, {already_started, erlexec}}} -> 
            ok;
        {error, Reason} ->
            % If erlexec fails to start, log it but don't fail the test
            % as the tests are primarily testing parameter validation
            io:format("Warning: Could not start erlexec application: ~p~n", [Reason]),
            ok
    end.

teardown_git_test(_) ->
    % Clean up test mode
    application:unset_env(devout, test_mode),
    application:unset_env(devout, git_timeout),
    ok.

%% Core Git operation tests - Removed no-op tests, keeping only meaningful ones

% All the individual core git operation tests were just placeholders with ?assertEqual(ok, ok)
% so they have been removed. Real git testing would require a git repository and is better
% done as integration tests.

%% Parameter validation tests - These are SAFE and provide real value

parameter_validation_test() ->
    % Test that handle functions validate parameters correctly (these don't execute git)
    ?assertEqual(<<"Error: git add requires 'files' (array) or 'file' (string) parameter">>,
                 devout_git:handle_git_add(#{<<"invalid">> => <<"value">>})),
    
    ?assertEqual(<<"Error: git pull requires 'remote' and 'branch' parameters">>,
                 devout_git:handle_git_pull(#{<<"invalid">> => <<"value">>})),
    
    ?assertEqual(<<"Error: git checkout branch requires 'branch' parameter">>,
                 devout_git:handle_git_checkout_branch(#{<<"invalid">> => <<"value">>})),
    
    ?assertEqual(<<"Error: git push requires 'remote' parameter">>,
                 devout_git:handle_git_push(#{<<"invalid">> => <<"value">>})),
    
    ?assertEqual(<<"Error: git commit all requires 'message' parameter">>,
                 devout_git:handle_git_commit_all(#{<<"invalid">> => <<"value">>})),
    
    ?assertEqual(<<"Error: git commit files requires 'files' (array) and 'message' parameters">>,
                 devout_git:handle_git_commit_files(#{<<"invalid">> => <<"value">>})),
    
    ?assertEqual(<<"Error: git clone requires 'url' parameter">>,
                 devout_git:handle_git_clone(#{<<"invalid">> => <<"value">>})).

%% Test module exports and function existence
module_structure_test() ->
    % Test that the module exports the expected functions
    Exports = devout_git:module_info(exports),
    ExpectedFunctions = [
        {add, 1}, {log, 0}, {log, 1}, {diff, 0}, {diff, 1}, {diff, 2},
        {pull, 2}, {checkout_branch, 1}, {push, 1}, {commit_all, 1},
        {commit_files, 2}, {clone, 1}, {status, 0},
        {handle_git_add, 1}, {handle_git_log, 1}, {handle_git_diff, 1},
        {handle_git_pull, 1}, {handle_git_checkout_branch, 1},
        {handle_git_push, 1}, {handle_git_commit_all, 1},
        {handle_git_commit_files, 1}, {handle_git_clone, 1},
        {handle_git_status, 1}
    ],
    lists:foreach(fun(Func) ->
        ?assert(lists:member(Func, Exports))
    end, ExpectedFunctions).

%% Test execute_git function - removed no-op placeholder

%% Integration test placeholder - module structure validation only

integration_test_() ->
    {setup,
     fun setup_git_test/0,
     fun teardown_git_test/1,
     [
         ?_test(module_structure_test())
     ]}.