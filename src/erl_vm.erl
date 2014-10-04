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

-export([register_receiver/2,
        send_to_vm/2]).

-record(s, {port = undefined, buf = [],
            buftmr = undefined, receiver = undefined}).

register_receiver(Pid, ReceiverPid) when is_pid(Pid) ->
    gen_server:call(Pid, {register, ReceiverPid}).

send_to_vm(Pid, String) when is_binary(String) ->
    send_to_vm(Pid, binary_to_list(String));
send_to_vm(Pid, String) when is_pid(Pid) ->
    gen_server:call(Pid, {string, String}).

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
                         [{line, 1},
                          {args,
                           ["-name", list_to_binary([Node,"@127.0.0.1"]),
                            "-setcookie", Cookie]},
                          exit_status, stderr_to_stdout,
                          {parallelism, true}]) of
        Port when is_port(Port) -> {ok, #s{port = Port}};
        {'EXIT', Reason} -> {stop, Reason};
        Other -> {stop, Other}
    end.

handle_call({register, ReceiverPid}, _From, State) ->
    ?I("Receiver ~p registered~n", [ReceiverPid]),
    {reply, ok, State#s{receiver = ReceiverPid}};
handle_call({string, String}, _From, #s{port = Port} = State) ->
    true = erlang:port_command(Port, String),
    {reply, ok, State};
handle_call(Request, From, State) ->
    {stop, {unsupported_call, Request, From},
     {error, unsupported, Request, From},
     State}.

handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request},
     State}.

handle_info(send_buf, #s{buftmr = BufTmr} = State) ->
    if BufTmr /= undefined -> erlang:cancel_timer(BufTmr); true -> ok end,
    {noreply, State#s{
                buf = if is_pid(State#s.receiver) ->
                             State#s.receiver ! State#s.buf,
                             [];
                         true ->
                             ?I("Got ~s~n", [State#s.buf]),
                             State#s.buf
                      end,
                buftmr = undefined}
    };
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
