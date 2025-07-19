%%%-------------------------------------------------------------------
%%% @doc
%%% Devout MCP tool registration
%%% @end
%%%-------------------------------------------------------------------
-module(devout_register).
-export([file_tools/0, git_tools/0, resources/0, prompts/0])

-spec file_tools() -> ok.
file_tools() ->
    %% Add new-dir tool
    ok = erlmcp_stdio:add_tool(
        <<"new-dir">>,
        <<"Create a new directory">>,
        fun devout_server:handle_new_dir/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path for the directory to create">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),

    %% Add new-dirs tool (create directory with children)
    ok = erlmcp_stdio:add_tool(
        <<"new-dirs">>,
        <<"Create a directory with child directories">>,
        fun devout_server:handle_new_dirs/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Base directory path to create">>
                },
                <<"children">> => #{
                    <<"type">> => <<"array">>,
                    <<"items">> => #{<<"type">> => <<"string">>},
                    <<"description">> => <<"Child directory names to create">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),

    %% Add move tool
    ok = erlmcp_stdio:add_tool(
        <<"move">>,
        <<"Move or rename a file or directory">>,
        fun devout_server:handle_move/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"source">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path of source file/directory">>
                },
                <<"destination">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path of destination">>
                }
            },
            <<"required">> => [<<"source">>, <<"destination">>]
        }
    ),

    %% Add write tool
    ok = erlmcp_stdio:add_tool(
        <<"write">>,
        <<"Create or write to a file">>,
        fun devout_server:handle_write/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path for the file">>
                },
                <<"content">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Content to write to the file">>
                },
                <<"mode">> => #{
                    <<"type">> => <<"string">>,
                    <<"enum">> => [<<"write">>, <<"append">>],
                    <<"description">> => <<"Write mode: write (overwrite) or append">>,
                    <<"default">> => <<"write">>
                }
            },
            <<"required">> => [<<"path">>, <<"content">>]
        }
    ),

    %% Add read tool
    ok = erlmcp_stdio:add_tool(
        <<"read">>,
        <<"Read a file">>,
        fun devout_server:handle_read/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path of the file to read">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),

    %% Add show_cwd tool
    ok = erlmcp_stdio:add_tool(
        <<"show-cwd">>,
        <<"Show current working directory">>,
        fun devout_server:handle_show_cwd/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{},
            <<"required">> => []
        }
    ),

    %% Add change_cwd tool
    ok = erlmcp_stdio:add_tool(
        <<"change-cwd">>,
        <<"Change current working directory">>,
        fun devout_server:handle_change_cwd/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path to change to">>
                }
            },
            <<"required">> => [<<"path">>]
        }
    ),

    %% Add list-files tool
    ok = erlmcp_stdio:add_tool(
        <<"list-files">>,
        <<"List files and directories in a path">>,
        fun devout_server:handle_list_files/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"path">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Relative path to list (default: current directory)">>,
                    <<"default">> => <<".">>
                }
            },
            <<"required">> => []
        }
    ),
    ok.

-spec git_tools() -> ok.
git_tools() ->
    % Git add
    ok = erlmcp_stdio:add_tool(
        <<"git-add">>,
        <<"Add files to git staging area">>,
        fun devout_git:handle_git_add/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"files">> => #{
                    <<"type">> => <<"array">>,
                    <<"items">> => #{<<"type">> => <<"string">>},
                    <<"description">> => <<"List of files to add">>
                },
                <<"file">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Single file to add">>
                }
            },
            <<"oneOf">> => [
                #{<<"required">> => [<<"files">>]},
                #{<<"required">> => [<<"file">>]}
            ]
        }
    ),

    % Git log
    ok = erlmcp_stdio:add_tool(
        <<"git-log">>,
        <<"Show git commit history">>,
        fun devout_git:handle_git_log/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"args">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Additional arguments for git log">>
                }
            }
        }
    ),

    % Git diff
    ok = erlmcp_stdio:add_tool(
        <<"git-diff">>,
        <<"Show differences between commits, commit and working tree, etc">>,
        fun devout_git:handle_git_diff/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"ref">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Reference to diff against (e.g., HEAD~1)">>
                },
                <<"ref1">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"First reference for comparison">>
                },
                <<"ref2">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Second reference for comparison">>
                }
            }
        }
    ),

    % Git pull
    ok = erlmcp_stdio:add_tool(
        <<"git-pull">>,
        <<"Pull changes from remote repository with rebase">>,
        fun devout_git:handle_git_pull/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"remote">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Remote name (e.g., 'origin')">>
                },
                <<"branch">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Branch name to pull">>
                }
            },
            <<"required">> => [<<"remote">>, <<"branch">>]
        }
    ),

    % Git checkout branch
    ok = erlmcp_stdio:add_tool(
        <<"git-checkout-branch">>,
        <<"Create and checkout a new branch">>,
        fun devout_git:handle_git_checkout_branch/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"branch">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"New branch name">>
                }
            },
            <<"required">> => [<<"branch">>]
        }
    ),

    % Git push
    ok = erlmcp_stdio:add_tool(
        <<"git-push">>,
        <<"Push changes to remote repository">>,
        fun devout_git:handle_git_push/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"remote">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Remote name (e.g., 'origin')">>
                }
            },
            <<"required">> => [<<"remote">>]
        }
    ),

    % Git commit all
    ok = erlmcp_stdio:add_tool(
        <<"git-commit-all">>,
        <<"Commit all changes with a message">>,
        fun devout_git:handle_git_commit_all/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"message">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Commit message">>
                }
            },
            <<"required">> => [<<"message">>]
        }
    ),

    % Git commit files
    ok = erlmcp_stdio:add_tool(
        <<"git-commit-files">>,
        <<"Commit specific files with a message">>,
        fun devout_git:handle_git_commit_files/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"files">> => #{
                    <<"type">> => <<"array">>,
                    <<"items">> => #{<<"type">> => <<"string">>},
                    <<"description">> => <<"List of files to commit">>
                },
                <<"message">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Commit message">>
                }
            },
            <<"required">> => [<<"files">>, <<"message">>]
        }
    ),

    % Git clone
    ok = erlmcp_stdio:add_tool(
        <<"git-clone">>,
        <<"Clone a git repository">>,
        fun devout_git:handle_git_clone/1,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"url">> => #{
                    <<"type">> => <<"string">>,
                    <<"description">> => <<"Repository URL to clone">>
                }
            },
            <<"required">> => [<<"url">>]
        }
    ),

    % Git status
    ok = erlmcp_stdio:add_tool(
        <<"git-status">>,
        <<"Show git repository status">>,
        fun devout_git:handle_git_status/1,
        #{}
    ),

    ok.

-spec resources() -> ok.
resources() ->
    %% Add status resource
    ok = erlmcp_stdio:add_resource(
        <<"devout://status">>,
        <<"Devout server status and configuration">>,
        fun(_Uri) ->
            Status = #{
                status => running,
                base_dir => list_to_binary(element(2, file:get_cwd())),
                config => #{
                    max_file_size => application:get_env(devout, max_file_size, 10485760),
                    allowed_extensions => application:get_env(devout, allowed_extensions, all)
                }
            },
            jsx:encode(Status, [pretty])
        end,
        <<"application/json">>
    ),
    %% Add help resource
    ok = erlmcp_stdio:add_resource(
        <<"devout://help">>,
        <<"Devout Help">>,
        fun devout_server:handle_help_resource/1,
        <<"text/plain">>
    ).
    ok.

-spec prompts() -> ok.
prompts() ->
    ok = erlmcp_stdio:add_prompt(
        <<"create_project_with_git">>,
        <<"Generate a prompt for creating a new project with git initialization">>,
        fun(#{<<"project_name">> := Name, <<"project_type">> := Type}) ->
            ProjectPrompt = case Type of
                <<"erlang">> ->
                    <<"Create an Erlang/OTP project named '", Name/binary, "' with:\n",
                      "1. Standard directories: src, include, test, priv\n",
                      "2. rebar.config file with dependencies\n",
                      "3. Application file (.app.src)\n",
                      "4. Supervisor and main application modules\n",
                      "5. Initialize git repository\n",
                      "6. Create .gitignore for Erlang projects\n",
                      "7. Add initial commit\n",
                      "Use the devout file and git tools to complete this setup.">>;
                <<"web">> ->
                    <<"Create a web project named '", Name/binary, "' with:\n",
                      "1. Frontend structure: src, public, assets\n",
                      "2. package.json with basic dependencies\n",
                      "3. HTML, CSS, and JavaScript starter files\n",
                      "4. Initialize git repository\n",
                      "5. Create appropriate .gitignore\n",
                      "6. Add initial commit\n",
                      "Use the devout file and git tools to complete this setup.">>;
                _ ->
                    <<"Create a ", Type/binary, " project named '", Name/binary, "' with:\n",
                      "1. Appropriate directory structure\n",
                      "2. Basic configuration files\n",
                      "3. Initialize git repository\n",
                      "4. Create .gitignore file\n",
                      "5. Add initial commit\n",
                      "Use the devout file and git tools to complete this setup.">>
            end,
            ProjectPrompt;
           (_) ->
            <<"Error: create_project_with_git requires 'project_name' and 'project_type' parameters">>
        end,
        [
            #{<<"name">> => <<"project_name">>, <<"description">> => <<"Name of the project">>, <<"required">> => true},
            #{<<"name">> => <<"project_type">>, <<"description">> => <<"Type of project (erlang, web, library, etc.)">>, <<"required">> => true}
        ]
    ),
    ok.
