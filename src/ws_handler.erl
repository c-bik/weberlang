-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-include("weberlang.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(IW(__F, __A), ?I("[~p] "__F, [?LINE|__A])).
init({tcp, http}, _Req, _Opts) ->
    ?IW("init ~p~n~p~n", [_Req, _Opts]),
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?IW("websocket_init ~p~n~p~n~p~n", [_TransportName, Req, _Opts]),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    ?IW("websocket_handle ~p~n~p~n~p~n", [Msg, Req, State]),
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    ?IW("websocket_handle ~p~n~p~n~p~n", [_Data, Req, State]),
	{ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    ?IW("websocket_info ~p~n~p~n~p~n~p~n", [_Ref, Msg, Req, State]),
	erlang:start_timer(1000, self(), <<"How' you doin'?">>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    ?IW("websocket_info ~p~n~p~n~p~n", [_Info, Req, State]),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ?IW("websocket_terminate ~p~n~p~n~p~n", [_Reason, _Req, _State]),
	ok.
