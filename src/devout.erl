%%%-------------------------------------------------------------------
%%% @doc
%%% Devout main entry point - Simple OTP application startup
%%% @end
%%%-------------------------------------------------------------------
-module(devout).
-export([start/0]).

%%====================================================================
%% API
%%====================================================================

start() ->
    %% Simply start the devout application - OTP will handle dependencies
    application:ensure_all_started(devout).