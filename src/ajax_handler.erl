-module(ajax_handler).
-behaviour(cowboy_loop_handler).

-include("weberlang.hrl").

-export([init/3,info/3,terminate/3]).

init({tcp, http}, Req, []) ->
    display_req(Req),
    {Cmd, Req1} = cowboy_req:path_info(Req),
    {ok, JsonStr, Req2} = cowboy_req:body(Req1),
    cmd(Cmd, JsonStr),
    {loop, Req2, <<>>, 5000, hibernate}.

terminate(_Reason, _Req, _State) ->
	ok.

info(Resp, Req, State) ->
	{ok, Req1} = cowboy_req:reply(200, [
          {<<"content-encoding">>, <<"utf-8">>}
        , {<<"content-type">>, <<"application/json">>}
        ], Resp, Req),
    {loop, Req1, State, hibernate}.

-define(ERR(__T),
        #{<<"result">> => <<"error">>,
          <<"message">> => err(__T)}).
err(Term) when is_binary(Term) -> Term;
err(Term) when is_list(Term) ->
    list_to_binary(
      case catch io_lib:format("~s", [Term]) of
          {'EXIT', _} -> io_lib:format("~p", [Term]);
          R -> R
      end);
err(Term) ->
    list_to_binary(io_lib:format("~p", [Term])).

cmd([Cmd], JsonStr) when is_binary(JsonStr) ->
    Req = self(),
    spawn(fun() ->
            Req ! jsxn:encode(cmd(Cmd, jsxn:decode(JsonStr)))
          end);
cmd(<<"styles">>, #{<<"path">> := Path}) ->
    try
        [_|PartsInReverse] = lists:reverse(filename:split(binary_to_list(Path))),
        StylePath = filename:join([?PRIVDIR | lists:reverse(PartsInReverse)]),
        #{<<"result">> => <<"ok">>,
          <<"styles">> => [list_to_binary(F) || F <- filelib:wildcard("*.css", StylePath)]}
    catch
        _:Error ->
            ?ERR(Error)
    end;
cmd(<<"start_vm">>, #{<<"node">> := Node, <<"cookie">> := Cookie}) ->
    case erl_vm:start(Node, Cookie) of
        {ok, VMControllerPid} ->
            ?I("Start VM: Node ~p, Cookie ~p, Child ~p~n",
               [Node, Cookie, VMControllerPid]),
            #{<<"result">> => <<"ok">>,
              <<"vm_controller">> =>
                base64:encode(term_to_binary(VMControllerPid))};
        {error, Reason} ->
            ?ERR(Reason)
    end;
cmd(Cmd, Args) ->
    ?I("UNSUPPORTED Cmd ~p Args ~p~n", [Cmd, Args]),
    ?ERR(list_to_binary(["Unsupported ", Cmd])).

display_req(Req) ->
    ?I("~n-------------------------------------------------------~n"),
    try
        ?I("method     ~p~n", [element(1,cowboy_req:method(Req))]),
        ?I("version    ~p~n", [element(1,cowboy_req:version(Req))]),
        ?I("peer       ~p~n", [element(1,cowboy_req:peer(Req))]),
        %?I("peer_addr  ~p~n", [element(1,cowboy_req:peer_addr(Req))]),
        ?I("host       ~p~n", [element(1,cowboy_req:host(Req))]),
        ?I("host_info  ~p~n", [element(1,cowboy_req:host_info(Req))]),
        ?I("port       ~p~n", [element(1,cowboy_req:port(Req))]),
        ?I("path       ~p~n", [element(1,cowboy_req:path(Req))]),
        ?I("path_info  ~p~n", [element(1,cowboy_req:path_info(Req))]),
        ?I("qs         ~p~n", [element(1,cowboy_req:qs(Req))]),
        %?I("qs_val     ~p~n", [element(1,cowboy_req:qs_val(Req))]),
        %?I("qs_vals    ~p~n", [element(1,cowboy_req:qs_vals(Req))]),
        %?I("fragment   ~p~n", [element(1,cowboy_req:fragment(Req))]),
        ?I("host_url   ~p~n", [element(1,cowboy_req:host_url(Req))]),
        ?I("url        ~p~n", [element(1,cowboy_req:url(Req))]),
        %?I("binding    ~p~n", [element(1,cowboy_req:binding(Req))]),
        ?I("bindings   ~p~n", [element(1,cowboy_req:bindings(Req))]),
        ?I("hdr(ddls)  ~p~n", [element(1,cowboy_req:header(<<"dderl-session">>,Req))]),
        ?I("hdr(host)  ~p~n", [element(1,cowboy_req:header(<<"host">>,Req))]),
        %?I("headers    ~p~n", [element(1,cowboy_req:headers(Req))]),
        %?I("cookie     ~p~n", [element(1,cowboy_req:cookie(Req))]),
        ?I("cookies    ~p~n", [element(1,cowboy_req:cookies(Req))]),
        %?I("meta       ~p~n", [element(1,cowboy_req:meta(Req))]),
        ?I("has_body   ~p~n", [cowboy_req:has_body(Req)]),
        ?I("body_len   ~p~n", [element(1,cowboy_req:body_length(Req))]),
        ?I("body_qs    ~p~n", [element(2,cowboy_req:body_qs(Req))]),
        ?I("body       ~p~n", [element(2,cowboy_req:body(Req))])
    catch
        _:Reason -> ?I("display_req Error : ~p~n", [Reason])
    end,
    ?I("-------------------------------------------------------~n").
