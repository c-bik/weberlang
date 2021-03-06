-module(weberlang).
-behaviour(application).

-include("weberlang.hrl").

%% Application callbacks
-export([start/0, start/2, stop/1]).

% cowboy callbacks
%% API.
-export([ init/3
        , handle/2
        , terminate/3
        ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================


start() ->
    %ok = application:start(sasl),
    ssl:start(),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    lager:start(),
    ok = application:start(?MODULE).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", ?MODULE, []},
               {"/ws/[...]", ws_handler, []},
               {"/rest/[...]", ajax_handler, []},
               {"/[...]", cowboy_static, {dir, ?PRIVDIR}}]}
    ]),
    Ip = {0,0,0,0},
    Port = 8080,
    case cowboy:start_http(my_http_listener, 100,
                           [{ip, Ip}, {port, Port}],
                           [{env, [{dispatch, Dispatch}]}]) of
        {ok, _} ->
            lager:info("starting ~p priv ~s url http://~s:~p",
                       [?MODULE, filename:absname(?PRIVDIR),
                        inet:ntoa(Ip), Port]);
        {error, Error} ->
            lager:error("error starting ~p priv ~s url http://~s:~p reason ~p",
                        [?MODULE, filename:absname(?PRIVDIR), inet:ntoa(Ip), Port, Error])
    end,
    weberlang_sup:start_link().

stop(_State) ->
    lager:info("stopping ~p", [?MODULE]),
    ok.

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    Filename = filename:join([?PRIVDIR, "index.html"]),
	{ok, Html} = file:read_file(Filename),
	{ok, Req2} = cowboy_req:reply(200,
		[{<<"content-type">>, <<"text/html">>}],
		Html, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
