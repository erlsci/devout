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
    %% Start the devout application - OTP will handle dependencies and start phases
    application:ensure_all_started(devout).