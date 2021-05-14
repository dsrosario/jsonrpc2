-module(jsonrpc2_test_utils).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

handler(<<"sum">>, List, _) -> {ok, lists:sum(List)};
handler(<<"event">>, _, _) -> {ok, #{}};
handler(<<"invalid_params">>, _, _) -> {error, invalid_params}.

build_request(Method, Params, Id) ->
    Base = #{jsonrpc => <<"2.0">>, 
             method => Method, 
             params => Params},
    case Id of
        undefined -> Base;
        _ -> maps:put(id, Id, Base)
    end.

validate_response(DecodedResponses, ExpectedIds) when is_list(DecodedResponses) andalso is_list(ExpectedIds) ->
    lists:foreach(
        fun({DecodedResponse, ExpectedId}) -> validate_response(DecodedResponse, ExpectedId) end,
        lists:zip(DecodedResponses, ExpectedIds));
validate_response(DecodedResponse, ExpectedId) when is_map(DecodedResponse) ->
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedResponse)),
    ?assertEqual(ExpectedId, maps:get(<<"id">>, DecodedResponse)),
    ok.

validate_error_code(Response, ErrorCode) -> 
    Error = maps:get(<<"error">>, Response),
    ?assertEqual(ErrorCode, maps:get(<<"code">>, Error)).

do_json_rpc(Request) ->
    case jsonrpc2:handle(jsx:encode(Request), fun handler/3) of
        {reply, Data} -> jsx:decode(Data, [return_maps]);
        noreply -> undefined
    end.

