-module(enspector_page).
-export([getResourceTree/1]).

getResourceTree(Args) ->
    {struct, [{<<"frameTree">>, []}]}.
