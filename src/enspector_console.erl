-module(enspector_console).
-export([enable/1]).

enable([]) ->
    {struct, [{<<"expiredMessagesCount">>, 0}]}.
