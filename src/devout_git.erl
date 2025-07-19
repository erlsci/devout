%% @doc Git operations module for devout
%% 
%% This module provides MCP tools for common Git operations using erlexec
%% to execute Git commands safely.
%%
%% Supported commands:
%% - git add <files>
%% - git log [options]  
%% - git diff [ref1] [ref2]
%% - git pull <remote> <branch> --rebase
%% - git checkout -b <name>
%% - git push <remote>
%% - git commit -a -m <message>
%% - git commit <files> -m <message>
%% - git clone <url>

-module(devout_git).

-include("devout.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    % Core git operations
    add/1,
    log/0, log/1,
    diff/0, diff/1, diff/2,
    pull/2,
    checkout_branch/1,
    push/1,
    commit_all/1,
    commit_files/2,
    clone/1,
    status/0,
    
    % MCP tool handlers
    handle_git_add/1,
    handle_git_log/1,
    handle_git_diff/1,
    handle_git_pull/1,
    handle_git_checkout_branch/1,
    handle_git_push/1,
    handle_git_commit_all/1,
    handle_git_commit_files/1,
    handle_git_clone/1,
    handle_git_status/1
]).

%% @doc Execute git add command for specified files
-spec add([binary()] | binary()) -> git_result().
add(Files) when is_list(Files) ->
    FileArgs = [binary_to_list(F) || F <- Files],
    execute_git(["add"] ++ FileArgs);
add(File) when is_binary(File) ->
    execute_git(["add", binary_to_list(File)]).

%% @doc Execute git log with no arguments (default format)
-spec log() -> git_result().
log() ->
    execute_git(["log", "--oneline", "-10"]).

%% @doc Execute git log with custom arguments
-spec log(binary() | string()) -> git_result().
log(Args) when is_binary(Args) ->
    log(binary_to_list(Args));
log(Args) when is_list(Args) ->
    % Split args string into individual arguments
    ArgList = string:tokens(Args, " "),
    execute_git(["log"] ++ ArgList).

%% @doc Execute git diff with no arguments (working directory vs staged)
-spec diff() -> git_result().
diff() ->
    execute_git(["diff"]).

%% @doc Execute git diff with one argument (commit/branch vs working directory)
-spec diff(binary() | string()) -> git_result().
diff(Ref) when is_binary(Ref) ->
    diff(binary_to_list(Ref));
diff(Ref) when is_list(Ref) ->
    execute_git(["diff", Ref]).

%% @doc Execute git diff with two arguments (compare two refs)
-spec diff(binary() | string(), binary() | string()) -> git_result().
diff(Ref1, Ref2) when is_binary(Ref1), is_binary(Ref2) ->
    diff(binary_to_list(Ref1), binary_to_list(Ref2));
diff(Ref1, Ref2) when is_list(Ref1), is_list(Ref2) ->
    execute_git(["diff", Ref1, Ref2]).

%% @doc Execute git pull with rebase
-spec pull(binary() | string(), binary() | string()) -> git_result().
pull(Remote, Branch) when is_binary(Remote), is_binary(Branch) ->
    pull(binary_to_list(Remote), binary_to_list(Branch));
pull(Remote, Branch) when is_list(Remote), is_list(Branch) ->
    execute_git(["pull", Remote, Branch, "--rebase"]).

%% @doc Create and checkout a new branch
-spec checkout_branch(binary() | string()) -> git_result().
checkout_branch(BranchName) when is_binary(BranchName) ->
    checkout_branch(binary_to_list(BranchName));
checkout_branch(BranchName) when is_list(BranchName) ->
    execute_git(["checkout", "-b", BranchName]).

%% @doc Push to remote
-spec push(binary() | string()) -> git_result().
push(Remote) when is_binary(Remote) ->
    push(binary_to_list(Remote));
push(Remote) when is_list(Remote) ->
    execute_git(["push", Remote]).

%% @doc Commit all changes with message
-spec commit_all(binary() | string()) -> git_result().
commit_all(Message) when is_binary(Message) ->
    commit_all(binary_to_list(Message));
commit_all(Message) when is_list(Message) ->
    execute_git(["commit", "-a", "-m", Message]).

%% @doc Commit specific files with message
-spec commit_files([binary()] | [string()], binary() | string()) -> git_result().
commit_files(Files, Message) when is_binary(Message) ->
    commit_files(Files, binary_to_list(Message));
commit_files(Files, Message) when is_list(Files), is_list(Message) ->
    FileArgs = [if is_binary(F) -> binary_to_list(F); true -> F end || F <- Files],
    execute_git(FileArgs ++ ["-m", Message]).

%% @doc Clone a repository
-spec clone(binary() | string()) -> git_result().
clone(Url) when is_binary(Url) ->
    clone(binary_to_list(Url));
clone(Url) when is_list(Url) ->
    execute_git(["clone", Url]).

%% @doc Get git status
-spec status() -> git_result().
status() ->
    execute_git(["status", "--porcelain"]).

%% @doc Execute git command safely using erlexec
-spec execute_git([string()]) -> git_result().
execute_git(Args) ->
    Cmd = "git",
    Timeout = application:get_env(devout, git_timeout, ?DEFAULT_GIT_TIMEOUT),
    Options = [
        {stdout, self()},
        {stderr, self()},
        monitor
    ],
    
    try
        ?LOG_INFO("Executing git command: ~p ~p", [Cmd, Args]),
        case exec:run([Cmd | Args], Options) of
            {ok, _, OsPid} ->
                collect_output(OsPid, <<>>, <<>>, Timeout);
            {error, Reason} ->
                ?LOG_ERROR("Failed to execute git command: ~p", [Reason]),
                {error, Reason}
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Exception executing git command: ~p:~p~n~p", 
                      [Class, Reason, Stacktrace]),
            {error, {exception, Class, Reason}}
    end.

%% @doc Collect output from erlexec process
-spec collect_output(integer(), binary(), binary(), timeout()) -> git_result().
collect_output(OsPid, StdOut, StdErr, Timeout) ->
    receive
        {stdout, OsPid, Data} ->
            collect_output(OsPid, <<StdOut/binary, Data/binary>>, StdErr, Timeout);
        {stderr, OsPid, Data} ->
            collect_output(OsPid, StdOut, <<StdErr/binary, Data/binary>>, Timeout);
        {'DOWN', _Ref, process, {_Exec, OsPid}, normal} ->
            case byte_size(StdErr) of
                0 ->
                    {ok, StdOut};
                _ ->
                    ?LOG_WARNING("Git command produced stderr: ~s", [StdErr]),
                    {ok, StdOut}
            end;
        {'DOWN', _Ref, process, {_Exec, OsPid}, {exit_status, ExitCode}} ->
            ?LOG_ERROR("Git command failed with exit code ~p: ~s", [ExitCode, StdErr]),
            {error, {exit_code, ExitCode, StdErr}}
    after
        Timeout ->
            ?LOG_ERROR("Git command timed out after ~p ms", [Timeout]),
            exec:kill(OsPid, 9),
            {error, timeout}
    end.

%% MCP Tool Handlers

%% @doc Handle git add MCP tool
-spec handle_git_add(map()) -> binary().
handle_git_add(#{<<"files">> := Files}) when is_list(Files) ->
    case add(Files) of
        {ok, Output} ->
            <<"Git add successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git add failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git add error: ~p", [Reason]))
    end;
handle_git_add(#{<<"file">> := File}) ->
    case add(File) of
        {ok, Output} ->
            <<"Git add successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git add failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git add error: ~p", [Reason]))
    end;
handle_git_add(_) ->
    <<"Error: git add requires 'files' (array) or 'file' (string) parameter">>.

%% @doc Handle git log MCP tool
-spec handle_git_log(map()) -> binary().
handle_git_log(#{<<"args">> := Args}) ->
    case log(Args) of
        {ok, Output} ->
            <<"Git log:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git log failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git log error: ~p", [Reason]))
    end;
handle_git_log(_) ->
    case log() of
        {ok, Output} ->
            <<"Git log:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git log failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git log error: ~p", [Reason]))
    end.

%% @doc Handle git diff MCP tool
-spec handle_git_diff(map()) -> binary().
handle_git_diff(#{<<"ref1">> := Ref1, <<"ref2">> := Ref2}) ->
    case diff(Ref1, Ref2) of
        {ok, Output} ->
            <<"Git diff:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git diff failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git diff error: ~p", [Reason]))
    end;
handle_git_diff(#{<<"ref">> := Ref}) ->
    case diff(Ref) of
        {ok, Output} ->
            <<"Git diff:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git diff failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git diff error: ~p", [Reason]))
    end;
handle_git_diff(_) ->
    case diff() of
        {ok, Output} ->
            <<"Git diff:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git diff failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git diff error: ~p", [Reason]))
    end.

%% @doc Handle git pull MCP tool
-spec handle_git_pull(map()) -> binary().
handle_git_pull(#{<<"remote">> := Remote, <<"branch">> := Branch}) ->
    case pull(Remote, Branch) of
        {ok, Output} ->
            <<"Git pull successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git pull failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git pull error: ~p", [Reason]))
    end;
handle_git_pull(_) ->
    <<"Error: git pull requires 'remote' and 'branch' parameters">>.

%% @doc Handle git checkout branch MCP tool
-spec handle_git_checkout_branch(map()) -> binary().
handle_git_checkout_branch(#{<<"branch">> := Branch}) ->
    case checkout_branch(Branch) of
        {ok, Output} ->
            <<"Git checkout branch successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git checkout branch failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git checkout branch error: ~p", [Reason]))
    end;
handle_git_checkout_branch(_) ->
    <<"Error: git checkout branch requires 'branch' parameter">>.

%% @doc Handle git push MCP tool
-spec handle_git_push(map()) -> binary().
handle_git_push(#{<<"remote">> := Remote}) ->
    case push(Remote) of
        {ok, Output} ->
            <<"Git push successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git push failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git push error: ~p", [Reason]))
    end;
handle_git_push(_) ->
    <<"Error: git push requires 'remote' parameter">>.

%% @doc Handle git commit all MCP tool
-spec handle_git_commit_all(map()) -> binary().
handle_git_commit_all(#{<<"message">> := Message}) ->
    case commit_all(Message) of
        {ok, Output} ->
            <<"Git commit all successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git commit all failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git commit all error: ~p", [Reason]))
    end;
handle_git_commit_all(_) ->
    <<"Error: git commit all requires 'message' parameter">>.

%% @doc Handle git commit files MCP tool
-spec handle_git_commit_files(map()) -> binary().
handle_git_commit_files(#{<<"files">> := Files, <<"message">> := Message}) when is_list(Files) ->
    case commit_files(Files, Message) of
        {ok, Output} ->
            <<"Git commit files successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git commit files failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git commit files error: ~p", [Reason]))
    end;
handle_git_commit_files(_) ->
    <<"Error: git commit files requires 'files' (array) and 'message' parameters">>.

%% @doc Handle git clone MCP tool
-spec handle_git_clone(map()) -> binary().
handle_git_clone(#{<<"url">> := Url}) ->
    case clone(Url) of
        {ok, Output} ->
            <<"Git clone successful:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git clone failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git clone error: ~p", [Reason]))
    end;
handle_git_clone(_) ->
    <<"Error: git clone requires 'url' parameter">>.

%% @doc Handle git status MCP tool  
-spec handle_git_status(map()) -> binary().
handle_git_status(_) ->
    case status() of
        {ok, Output} ->
            <<"Git status:\n", Output/binary>>;
        {error, {exit_code, Code, Error}} ->
            iolist_to_binary(io_lib:format("Git status failed (exit code ~p): ~s", [Code, Error]));
        {error, Reason} ->
            iolist_to_binary(io_lib:format("Git status error: ~p", [Reason]))
    end.
