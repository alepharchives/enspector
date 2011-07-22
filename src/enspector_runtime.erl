-module(enspector_runtime).
-export([evaluate/1]).
-compile(export_all).

-record(evaluationRequest, {expression, objectGroup, includeCommandLineAPI}).
-record(remoteObject, {id, type, className, description, hasChildren}).

handle_command(<<"evaluate">>, Params) ->
    Exp = proplists:get_value(<<"expression">>, Params),
    InCmdlAPI = proplists:get_value(<<"includeCommandLineAPI">>, Params),
    ObjGroup = proplists:get_value(<<"objectGroup">>, Params),
    DoNotPauseOnExceptions = proplists:get_value(<<"doNotPauseOnExceptions">>, Params),
    evaluateAndWrap(Exp, ObjGroup, InCmdlAPI).

evaluate(Req) when is_list(Req) ->
    evaluate(parse(Req));
evaluate(#evaluationRequest{expression=Exp, objectGroup=Og,
                            includeCommandLineAPI=Ila}) ->
    evaluateAndWrap(Exp, Og, Ila).

parse(Req) ->
    Exp = proplists:get_value(<<"expression">>, Req),
    Og = proplists:get_value(<<"objectGroup">>, Req),
    Icl = proplists:get_value(<<"includeCommandLineAPI">>, Req),
    #evaluationRequest{expression=Exp,
                       objectGroup=Og,
                       includeCommandLineAPI=Icl}.

eval(B) when is_binary(B) ->
    eval(binary_to_list(B));
eval(S) when is_list(S) ->
    {ok, Scanned, _} = erl_scan:string(S++"."),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value, _} = erl_eval:exprs(Parsed, []),
    io:format("eval: ~p~n", [Value]),
    Value.

evaluateAndWrap(Exp, ObjectGroup, InjectCommandLineAPI) ->
    try eval(Exp) of
        Value ->
            {struct, [
                      {<<"result">>, wrapObject(Value, ObjectGroup)}]}
    catch T:E ->
            {struct, [{<<"wasThrown">>, true},
                      {<<"result">>, wrapException(E, T)}]}
    end.


wrapObject(Value, ObjectGroup) when is_number(Value) ->
    Description = describe(Value),
    json(#remoteObject{type=number, description=Description});
wrapObject(Value, ObjectGroup) ->
    ObjectId=object_id(Value),
    Type = type(Value),
    Description = describe(Value),
    json(#remoteObject{id=ObjectId, type=Type, className=Type,
                       description=Description, hasChildren=false}).

json(R) when is_record(R, remoteObject) ->
    Fields =
        [
         {<<"id">>, R#remoteObject.id},
         {<<"type">>, R#remoteObject.type},
         {<<"className">>, R#remoteObject.className},
         {<<"description">>, R#remoteObject.description},
         {<<"hasChildren">>, R#remoteObject.hasChildren}
        ],
    Fs = lists:filter(fun({_,Y}) -> Y /= undefined end, Fields),
    {struct, Fs}.

wrapException(Exception, Type) ->
    Desc = list_to_binary("[ Exception: " ++ describe(Exception) ++ " ]"),
    json(#remoteObject{
            className=Type,
            hasChildren=false,
            type=object,
            description=Desc}).

object_id(Value) ->
    "not implemented".
             
show(X) when is_tuple(X) -> "{...}";
show(X) when is_list(X) -> "[...]";
show(X) when is_function(X) -> "#fun...";
show(X) when is_atom(X) -> atom_to_list(X);
show(X) when is_binary(X) -> binary_to_list(X);
show(X) when is_pid(X) -> pid_to_list(X);
show(X) when is_integer(X) -> integer_to_list(X);
show(X) when is_float(X) -> float_to_list(X);
show(X) when is_port(X) -> erlang:port_to_list(X);
show(X) when is_boolean(X) -> atom_to_list(X);
show(X) when is_reference(X) -> erlang:ref_to_list(X);
show(X) -> X.

describe_values(Values) when is_list(Values) ->
    Shown = lists:foldl(fun(X, Acc) -> Acc ++ [show(X)] end, [], Values),
    string:join(Shown, ", ").
             
describe(V) when is_tuple(V) ->
    list_to_binary("{ " ++ describe_values(tuple_to_list(V)) ++ " }");
describe(V) when is_atom(V) ->
    V;
describe(V) when is_function(V) ->
    Info = erlang:fun_info(V),
    V;
describe(V) when is_boolean(V) ->
    V;
describe(V) when is_list(V) ->
    list_to_binary("[ " ++ describe_values(V) ++ " ]");
describe(V) when is_binary(V) ->
    V;
describe(V) when is_integer(V) ->
    V;
describe(V) when is_float(V) ->
    V;
describe(V) when is_pid(V) ->
    list_to_binary(pid_to_list(V));
describe(V) when is_port(V) ->
    V;
describe(V) when is_reference(V) ->
    V;
describe(V) ->
    V.

type(V) when is_tuple(V) -> tuple;
type(V) when is_boolean(V) -> boolean;
type(V) when is_function(V) -> function;
type(V) when is_binary(V) -> binary;
type(V) when is_list(V) -> list;
type(V) when is_atom(V) -> atom;
type(V) when is_integer(V) -> integer;
type(V) when is_float(V) -> float;
type(V) when is_pid(V) -> pid;
type(V) when is_port(V) -> port;
type(V) when is_reference(V) -> reference.
