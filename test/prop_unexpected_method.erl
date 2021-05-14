-module(prop_unexpected_method).
-include_lib("proper/include/proper.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%
prop_test() ->
    ?FORALL(RequestJson, request_json(),
        begin
            {reply, Reply} = jsonrpc2:handle(jsx:encode(RequestJson), fun rpc_handler/3),
            ReplyJson = jsx:decode(Reply, [return_maps, {labels, atom}]),
            is_expected_reply(ReplyJson) andalso
            is_expected_id(RequestJson, ReplyJson)
        end).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
rpc_method() -> non_empty(utf8()).
rpc_id() -> oneof([integer(), non_empty(utf8()), null]).

request_json() ->
    ?LET({Method, Id}, {rpc_method(), rpc_id()},
          #{jsonrpc => <<"2.0">>,
            id => Id,
            method => Method,
            params => []}).

rpc_handler(<<>>, _, _) ->
    throw(unexpected_function_call).

is_expected_reply(#{jsonrpc := <<"2.0">>,
                    error := #{code := -32601}}) -> true;
is_expected_reply(_) -> false.

is_expected_id(#{id := Id}, #{id := Id}) -> true;
is_expected_id(_, _) -> false.
