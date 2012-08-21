-module(enspector_static_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).
-record(state, {fe_dir}).

init({_Any, http}, Req, []) ->
    {ok, Dir} = file:get_cwd(),
    {ok, Req, #state{fe_dir = Dir ++ "/front-end"}}.

handle(Req, State) ->
    {ok, FileBin} = get_static_content(Req, State#state.fe_dir),
    {ok, Req2} = cowboy_http_req:reply(200, [], FileBin, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

get_static_content(Req, Dir) ->
    {Paths, _} = cowboy_http_req:path_info(Req),
    Path = string:join([Dir | lists:map(fun binary_to_list/1, Paths)], "/"),
    io:format("Request static: ~p~n", [Path]),
    {ok, Bin} = file:read_file(Path).

