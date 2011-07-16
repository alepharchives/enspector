-module(enspector_static_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

-define(PREFIX, "/home/elihait/code/enspector/front-end").

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, FileBin} = get_static_content(Req), 
    {ok, Req2} = cowboy_http_req:reply(200, [], FileBin, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

get_static_content(Req) ->
    {Paths, _} = cowboy_http_req:path_info(Req),
    Path = string:join([?PREFIX|lists:map(fun binary_to_list/1, Paths)], "/"),
    io:format("Request static: ~p~n", [Path]),
    {ok, Bin} = file:read_file(Path).

