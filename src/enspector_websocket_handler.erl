-module(enspector_websocket_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3, websocket_terminate/3]).

-compile(export_all).

init({_Any, http}, Req, []) ->
    io:format("wh init~n"),
    case cowboy_http_req:header('Upgrade', Req) of
        {undefined, Req2} -> {ok, Req2, undefined};
        {<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
    end.

handle(Req, State) ->
    io:format("wh handle~n"),
    {ok, <<"nothing">>, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    io:format("wh ws init~n"),
    Req2 = cowboy_http_req:compact(Req),
    {ok, Req2, undefined}.

websocket_handle(tick, Req, State) ->
    {reply, <<"Tick">>, Req, State, hibernate};
websocket_handle({websocket, Msg}, Req, State) ->
    io:format("pid: ~p: wh ws handle: ~n~p~n", [self(),Msg]),
    Reply = enspector_backend:dispatch(Msg),
    {reply, Reply, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
