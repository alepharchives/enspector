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
    Reply = dispatch(Msg),
    {reply, Reply, Req, State, hibernate}.

dispatch(Msg) ->
    {Method, Params, Id} = parse(Msg),
    dispatch(Method, Params, Id).

websocket_terminate(_Reason, _Req, _State) ->
    ok.

parse(Msg) ->
    {struct, List} = mochijson2:decode(Msg),
    Method = proplists:get_value(<<"method">>, List),
    Params =
        case proplists:get_value(<<"params">>, List) of
            {struct, Xs} -> Xs;
            undefined -> []
        end,
    Id = proplists:get_value(<<"id">>, List),
    {Method, Params, Id}.

dispatch(<<"Console.enable">>, [], Id) ->
    pack_response(Id, ok);
dispatch(<<"Runtime.evaluate">>, Params, Id) ->
    Result = enspector_runtime:evaluate(Params),
    pack_response(Id, Result);
dispatch(_, _, Id) ->
    pack_response(Id, unimplemented).

-spec pack_response(CallId :: integer(), Result::term()) -> ok.
pack_response(CallId, Result) ->
    Json = mochijson2:encode({struct, [{<<"id">>, CallId}, {<<"result">>, Result}]}),
    iolist_to_binary(Json).
