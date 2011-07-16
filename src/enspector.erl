-module(enspector).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    application:start(cowboy),
    application:start(enspector).

start(_Type, _Args) ->
    Dispatch = [
		{'_', [
                       {[<<"enspector">>], enspector_websocket_handler, []},
                       {[<<"static">>, '...'], enspector_static_handler, []},
                       {'_', enspector_default_handler, []}
                      ]}
               ],
    cowboy:start_listener(http, 2,
                          cowboy_tcp_transport, [{port, 8080}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    enspector_sup:start_link().

stop(_State) ->
    ok.
