-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-include("weberlang.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(s, {erl_vm_pid = undfined}).

-define(IW(__F, __A), ?I("[~p] "__F, [?LINE|__A])).
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(tcp, Req, _Opts) ->
    {B64Pid, Req1} = cowboy_req:qs_val(<<"pid">>, Req),
    VMControllerPid = ?b64topid(B64Pid),
    ok = erl_vm:register_receiver(VMControllerPid, self()),
	{ok, Req1, #s{erl_vm_pid = VMControllerPid}}.

% Data From Browser (binary string)
websocket_handle({text, Msg}, Req, S) ->
    ok = erl_vm:send_to_vm(S#s.erl_vm_pid, Msg),
    %?IW("websocket_handle ~p~n", [Msg]),
    {ok, Req, S};
websocket_handle(_Data, Req, S) ->
    ?IW("websocket_handle ~p~n~p~n", [_Data, S]),
	{ok, Req, S}.

% Data To Browser (binary string)
websocket_info(Info, Req, S) when is_list(Info) ->
    ?IW("websocket_info ~p~n", [Info]),
	{reply, {text, list_to_binary(Info)},
     Req, S};
websocket_info(Info, Req, S) ->
    ?IW("websocket_info ~p~n~p~n", [Info, S]),
	{ok, Req, S}.

websocket_terminate(_Reason, _Req, S) ->
    ?IW("websocket_terminate ~p~n~p~n", [_Reason, S]),
	ok.
