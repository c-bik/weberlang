-module(weberlang).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(?MODULE).

start(_StartType, _StartArgs) ->
    weberlang_sup:start_link().

stop(_State) ->
    ok.
