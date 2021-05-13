-module(prop_invalid_input).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(InvalidBinary, utf8(),
        begin
            {reply, Reply} = jsonrpc2:handle(InvalidBinary, fun rpc_handler/2),
            Json = jsx:decode(Reply, [return_maps]),
            is_expected_reply(Json)
        end),
    ?FORALL(InvalidBinary, utf8(),
        begin
            {reply, Reply} = jsonrpc2:handle(InvalidBinary, fun rpc_handler/3),
            Json = jsx:decode(Reply, [return_maps]),
            is_expected_reply(Json)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
rpc_handler(_, _) ->
    throw(unexpected_function_call).

rpc_handler(_, _, _) ->
    throw(unexpected_function_call).

is_expected_reply(#{<<"jsonrpc">> := <<"2.0">>,
                    <<"error">> := #{<<"code">> := -32700}}) -> true;
is_expected_reply(_) -> false.
