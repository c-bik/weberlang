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

-record(s, {port = undefined, buf = [], buftmr = undefined}).

start(Node, Cookie) ->
    supervisor:start_child(weberlang_sup,
                           {Node,
                            {?MODULE, start_link,
                             [Node, Cookie]},
                            temporary, 5000, worker,
                            [?MODULE]}).

start_link(Node, Cookie) ->
    gen_server:start_link(?MODULE, [Node, Cookie], []).

init([Node, Cookie]) ->
    [_|Rest] = lists:reverse(filename:split(code:lib_dir())),
    ErtsBin = filename:join(lists:reverse(["bin"|Rest])),
    [Erl] = filelib:wildcard("erl{.exe,}", ErtsBin),
    ErlExe = filename:join(ErtsBin, Erl),
    ?I("Starting erlang VM at ~p, Node ~p, Cookie ~p~n",
       [ErlExe, Node, Cookie]),
    case catch open_port({spawn_executable, ErlExe},
                         [{line, 1}, {args, ["-sname", Node,
                                             "-setcookie", Cookie]},
                          exit_status,stderr_to_stdout,
                          {parallelism, true}]) of
        Port when is_port(Port) -> {ok, #s{port = Port}};
        {'EXIT', Reason} -> {stop, Reason};
        Other -> {stop, Other}
    end.

handle_call(Request, From, State) ->
    {stop, {unsupported_call, Request, From},
     {error, unsupported, Request, From},
     State}.

handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request},
     State}.

handle_info(send_buf, #s{buf = Buf, buftmr = BufTmr} = State) ->
    if BufTmr /= undefined -> erlang:cancel_timer(BufTmr); true -> ok end,
    ?I("Got ~s~n", [Buf]),
    {noreply, State#s{buf = [], buftmr = undefined}};
handle_info({P,{data,{T,Str}}},
            #s{port=P, buf = OldBuf, buftmr = BufTmr} = State) ->
    if BufTmr /= undefined -> erlang:cancel_timer(BufTmr); true -> ok end,
    NewBuf = OldBuf ++ Str ++ if T == eol -> "\n"; true -> "" end,
    NewBufTimer = erlang:send_after(100, self(), send_buf),
    {noreply, State#s{buf = NewBuf, buftmr = NewBufTimer}};
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
