-module(erl_vm).
-behaviour(gen_server).

-include("weberlang.hrl").

-export([start/2,
         start_link/2,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2
        ]).

-record(s, {}).

% via exports
-export([ register_name/2
          , register_name/3
          , unregister_name/1
          , whereis_name/1
          , send/2
        ]).

start(Node, Cookie) ->
    supervisor:start_child(weberlang_sup,
                           {{Node, Cookie},
                            {?MODULE, start_link,
                             [Node, Cookie]},
                            temporary, 5000, worker,
                            [?MODULE]}).

start_link(Node, Cookie) ->
    gen_server:start_link({via, ?MODULE, {Node, Cookie}},
                          ?MODULE, [Node, Cookie], []).

init([Node, Cookie]) ->
    %[_|Rest] = lists:reverse(filename:split(code:lib_dir())),
    %ErtsBin = filename:join(lists:reverse(["bin"|Rest])),
    %[Erl] = filelib:wildcard("erl{.exe,}", ErtsBin),
    %ErlExe = filename:join(ErtsBin, Erl),
    ErlExe = undefined,
    ?I("Starting erlang VM at ~p, Node ~p, Cookie ~p~n",
       [ErlExe, Node, Cookie]),
    {ok, #s{}}.

handle_call(Request, From, State) ->
    {stop, {unsupported_call, Request, From},
     {error, unsupported, Request, From},
     State}.

handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request},
     State}.

handle_info(Info, State) ->
    {stop, {unsupported_info, Info},
     State}.

terminate(Reason, State) ->
    {Reason, State}.

code_change(OldVsn, State, Extra) ->
    ?I("code_changed from ~p with ~p~n",
       [OldVsn, Extra]),
    {ok, State}.

format_status(Opt, [PDict, State]) ->
    {Opt, [PDict, State]}.

% VIA API
register_name(Name, Pid) ->
    ?I("Registering ~p with ~p~n", [Name, Pid]),
    global:register_name(Name, Pid).
register_name(Name, Pid, Resolve) ->
    ?I("Registering ~p with ~p~n", [Name, Pid]),
    global:register_name(Name, Pid, Resolve).
unregister_name(Name) -> global:unregister_name(Name).
whereis_name(Name) -> global:whereis_name(Name).
send(Name, Msg) -> global:send(Name, Msg).
