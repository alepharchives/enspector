-module(dispatcher_generator).

-compile(export_all).

-export([generate/1]).

generate(File) ->
    {ok, Bin} = file:read_file(File),
    Json = mochijson2:decode(Bin),
    {Types, Params, Returns, Events} =
        generate_domains(Json, {[], [], [], []}),
    {ok, Out} = file:open("src/enspector_parser.inc", [write]),

    lists:foreach(fun(T) -> file:write(Out, T) end, Types),
    ok = file:write(Out, <<"\n\n\n">>),

    lists:foreach(fun(P) -> file:write(Out, P) end, Params),
    ok = file:write(Out, <<"parse_params(D, M, P) ->
    {error, {unknown_params, D, M, P}}.\n\n\n">>),

    lists:foreach(fun(R) -> file:write(Out, R) end, Returns),
    ok = file:write(Out, <<"return_encoder(D, M) ->
    {error, {unknown_return, D, M}}.\n\n\n">>),

    ok = file:close(Out).

generate_domains([], {Types, Params, Returns, Events}) ->
    {lists:reverse(Types),
     lists:reverse(Params),
     lists:reverse(Returns),
     lists:reverse(Events)};
generate_domains([D|Rest], {Types, Params, Returns, Events}) ->
    {T, P, R, E} = generate_domain(D),
    generate_domains(Rest, {[T|Types], [P|Params], [R|Returns], [E|Events]}).

generate_domain({struct, DomainProps}) ->
    Domain = proplists:get_value(<<"domain">>, DomainProps),
    case Domain of
        %% Blacklist some types. The default order of CSS types defintion
        %% requires 2 passes to get every field type checked.
        %% TODO: implement a type dependency graph and output accordingly 
        <<"CSS">> ->
            {[], [], [], []};
        <<"DOM">> ->
            {[], [], [], []};
        _ ->
            Types =
                domain_types(Domain,
                             proplists:get_value(<<"types">>, DomainProps)),
            Commands = proplists:get_value(<<"commands">>, DomainProps),
            {Params, Returns} =
                domain_commands(Domain, Commands),
            Events = domain_events(
                       proplists:get_value(<<"events">>, DomainProps)),
            {Types, Params, Returns, Events}
    end.

domain_events(X) -> ok.

domain_types(Domain, undefined) ->
    [];
domain_types(Domain, Types) ->
    domain_types(Domain, Types, []).

domain_types(Domain, [], Acc) ->
    lists:reverse(Acc);
domain_types(Domain, [Type|Types], Acc) ->
    Record = domain_type(Domain, Type),
    domain_types(Domain, Types, [Record|Acc]).


domain_type(Domain, {struct, Props}) ->
    Name = proplists:get_value(<<"id">>, Props),
    RecordName = record_name(Domain, Name),
    case proplists:get_value(<<"type">>, Props) of
        <<"object">> ->
            TypeProps = proplists:get_value(<<"properties">>, Props),
            lists:flatten(
              [io_lib:format("-record(~p,{\n   ", [RecordName]),
               type_properties(Domain, TypeProps, []),
               "}).\n\n\n"]);
        Type ->
            ErlType = erl_type(Domain, binary_to_atom(Type, latin1)),
            add_name(RecordName),
            lists:flatten(
              io_lib:format("-type(~p() :: ~s).\n\n\n", [RecordName, ErlType]))
    end.

add_name(RecordName) ->
    io:format("put: ~p~n", [RecordName]),
    case get(new_types) of
        undefined ->
            put(new_types, [RecordName]);
        KnownTypes ->
            put(new_types, [RecordName|KnownTypes])
    end.

get_name(Name) ->
    case get(new_types) of
        undefined ->
            undefined;
        List ->
            case lists:member(Name, List) of
                true -> Name;
                false -> undefined
            end
    end.


type_properties(_, [], Acc) ->
    string:join(lists:reverse(Acc), "  ,");
type_properties(Domain, [{struct, P}|Ps], Acc) ->
    type_properties(Domain, Ps, [type_property(Domain, P)|Acc]).


type_property(Domain, P) ->
    Name = binary_to_atom(proplists:get_value(<<"name">>, P), latin1),
    Type =
        case proplists:get_value(<<"type">>, P) of
            undefined ->
                binary_to_atom(proplists:get_value(<<"$ref">>, P), latin1);
            T ->
                binary_to_atom(T, latin1)
        end,

    ErlType = erl_type(Domain, Type),

    case proplists:get_value(<<"optional">>, P) of
        true ->
            io_lib:format("~p = undefined :: ~s\n", [Name, ErlType]);
        _ ->
            io_lib:format("~p :: ~s\n", [Name, ErlType])
    end.

erl_type(_, object) -> "any()";
erl_type(_, string) -> "string()";
erl_type(_, number) -> "number()";
erl_type(_, integer) -> "integer()";
erl_type(_, array) -> "list()";
erl_type(_, boolean) -> "boolean()";
erl_type(Domain, Other) ->
    QualifiedType = 
        case binary:split(atom_to_binary(Other, latin1), <<".">>) of
            [_, _] -> 
                Other;
            _ ->
                OtherBin = atom_to_binary(Other, latin1), 
                Qualified = <<Domain/binary, ".", OtherBin/binary>>,
                binary_to_atom(Qualified, latin1)
        end,

    %% io:format("get: ~p = ~p~n", [QualifiedType, get_name(QualifiedType)]),
    TypeString =
        case get_name(QualifiedType) of
            undefined ->
                io_lib:format("#~p{}", [QualifiedType]);
            Type ->
                io_lib:format("~p()", [QualifiedType])
        end,
        
    lists:flatten(TypeString).
    
record_name(Domain, Name) ->
    binary_to_atom(<<Domain/binary, ".", Name/binary>>, latin1).

domain_commands(D, S) ->
    domain_commands(D, S, {[], []}).

domain_commands(D, [], {A, B}) ->
    {lists:reverse(A), lists:reverse(B)};
domain_commands(Domain, [Cmd|Rest], {A, B}) ->
    {P, R} = domain_command(Domain, Cmd),
    domain_commands(Domain, Rest, {[P|A], [R|B]}).

domain_command(Domain, {struct, CmdProps}) ->
    Method = proplists:get_value(<<"name">>, CmdProps),
    Params = proplists:get_value(<<"parameters">>, CmdProps),
    Returns = proplists:get_value(<<"returns">>, CmdProps),
    {command_params(Domain, Method, Params),
     command_returns(Domain, Method, Returns)}.

command_params(Domain, Method, undefined)->
    Header = io_lib:format("parse_params(~p, ~p, []) ->\n    {ok, []};\n",
                           [Domain, Method]),
    iolist_to_binary(lists:flatten(Header));
command_params(Domain, Method, Params) ->
    Header = io_lib:format("parse_params(~p, ~p, Params) ->\n    {ok,
     [\n", [Domain, Method]),
    ParamCheckers = 
        string:join(lists:map(
                      fun(X) -> check_paramter(X) end,
                      Params), ",\n"),
    iolist_to_binary(lists:flatten([Header, ParamCheckers, "\n     ]};\n"])).

check_paramter({struct, P}) ->
    Name = proplists:get_value(<<"name">>, P),
    case proplists:get_value(<<"optional">>, P) of
        undefined ->
            lists:flatten(io_lib:format("      {~p,
       case proplists:get_value(~p, Params) of
           undefined -> throw(illegal_arguments);
           V -> V
       end}", [Name, Name]));
        _ ->
            Line = "      {~p, proplists:get_value(~p, Params)}",
            lists:flatten(io_lib:format(Line, [Name, Name]))
    end.


command_returns(Domain, Method, undefined) ->
    Header = io_lib:format("return_encoder(~p, ~p) -> none;\n",
                           [Domain, Method]),
    iolist_to_binary(lists:flatten(Header));
command_returns(Domain, Method, Returns) ->
    Header = io_lib:format(
"return_encoder(~p, ~p) ->
    fun(Result) ->
        List = [\n", [Domain, Method]),

    ReturnCheckers =
        string:join(
          lists:map(fun(X) -> check_return(X) end, Returns),
          ",\n"),

    Tail = "\n                ],
        {struct, lists:filter(fun(X) -> X =/= \"\" end, List)}
    end;\n",

    iolist_to_binary(lists:flatten([Header, ReturnCheckers, Tail])).
    
check_return({struct, P}) ->
    Name = proplists:get_value(<<"name">>, P),
    case proplists:get_value(<<"optional">>, P) of
        undefined ->
            lists:flatten(io_lib:format(
"                  {~p,
                   case proplists:get_value(~p, Result) of
                       undefined -> throw(missing_return_argument);
                       V -> V
                   end}", [Name, Name]));
        _ ->
            lists:flatten(io_lib:format(
"                   case proplists:get_value(~p, Result) of
                       undefined -> [];
                       V -> {~p, V}
                   end", [Name, Name]))
    end.
