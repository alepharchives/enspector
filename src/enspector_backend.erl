-module(enspector_backend).
-export([dispatch/1]).

-include("enspector_parser.inc").

dispatch(Msg) ->
    case decode(Msg) of
        {error, {Id, Error}} ->
            log_warning("Decode failed. Id:~p, Error:~p~n", [Id, Error]),
            send_response(Id, true);
%%        {error, Error} ->
%%            log_warning("Decode failed. ~p~n", [Msg]),
%%            send_response(Id, true);
        {ok, {Id, Domain, Method, Params}} ->
            case domain_handler(Domain, Method) of
                non_existing ->
                    send_response(Id, non_existing_domain_handler);
                Module ->
                    case catch Module:handle_command(Method, Params) of
                        {'EXIT', Error} ->
                            log_warning("Failed handling command: ~p:~p~n",
                                       [Module, Method]),
                            send_response(Id, true);
                        Result ->
                            case return_encoder(Domain, Method) of
                                none ->
                                    send_response(Id, undefined);
                                Encoder ->
                                    {struct, Ts} = Result,
                                    send_response(Id, undefined, Encoder(Ts))
                            end
                    end
            end
    end.


domain_handler(Domain, _Method) ->
    Module = list_to_atom("enspector_" ++ string_lower(binary_to_list(Domain))),
    case code:which(Module) of
        non_existing ->
            non_existing;
        _ ->
            Module
    end.


string_lower(String) ->
    lists:map(fun(X) when X >= $A, X =< $Z ->
                      X + 32;
                 (X) ->
                      X
              end, String).


send_response(Id, Error) ->
    io:format("send_response: ~p ~p~n", [Id, Error]),
    Msg = {struct, [{<<"id">>, Id}, {<<"error">>, Error}]},
    iolist_to_binary(mochijson2:encode(Msg)).


send_response(Id, Error, Result) ->
    Msg = [{<<"id">>, Id}] ++
        case Error of
            [] ->
                [{<<"error">>, Error}];
            _ ->
                []
        end ++ [{<<"result">>, Result}],
    iolist_to_binary(mochijson2:encode({struct, Msg})).


decode(Msg) ->
    {struct, List} = mochijson2:decode(Msg),
    Id = proplists:get_value(<<"id">>, List),
    case parse_domain_method(List) of
        {ok, {Domain, Method}} ->
            ParamProp =
                case proplists:get_value(<<"params">>, List) of
                    {struct, Ps} ->
                        Ps;
                    undefined ->
                        []
                end,
            io:format("docode: ~p ~p ~p~n", [Domain, Method, ParamProp]),
            case catch parse_params(Domain, Method, ParamProp) of
                {ok, Params} ->
                    {ok, {Id, Domain, Method, Params}};
                E ->
                    {error, {Id, E}}
            end;
        {error, E} ->
            {error, {Id, E}}
    end.


parse_domain_method(List) ->
    case proplists:get_value(<<"method">>, List) of
        undefined ->
            {error, not_exists_domain_method};
        Domain_Method ->
            case binary:split(Domain_Method, <<".">>) of
                [Domain, Method] ->
                    {ok, {Domain, Method}};
                _ ->
                    {error, malformed_domain_method}
            end
    end.


log_warning(Fmt, Params) ->
    Msg = lists:flatten(io_lib:format(Fmt, Params)),
    io:format("WARN: ~p~n", [Msg]).
