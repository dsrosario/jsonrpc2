-module(jsonrpc2_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

suite() ->
     [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    application:ensure_all_started(jsonrpc2),
    Config.

end_per_suite(_Config) ->
    ok.
 
all() -> [
    simple_request,
    simple_event,
    batch_request,
    invalid_json,
    invalid_method,
    invalid_params,
    server_error,
    batch_request_with_opts
].

simple_request(_Config) ->
    lists:map(fun(Fun) -> 
        Request = jsonrpc2_test_utils:build_request(<<"sum">>, [1,2,3], 1),
        Response = jsonrpc2_test_utils:Fun(Request), 
        _ = jsonrpc2_test_utils:validate_response(Response, 1),
        ?assertEqual(6, maps:get(<<"result">>, Response)),
        ok
    end, [do_json_rpc, do_json_rpc_with_opts]).

simple_event(_Config) ->
    lists:map(fun(Fun) -> 
        Request = jsonrpc2_test_utils:build_request(<<"event">>, [], undefined),
        Response = jsonrpc2_test_utils:Fun(Request), 
        ?assertEqual(undefined, Response),
        ok
    end, [do_json_rpc, do_json_rpc_with_opts]).

batch_request(_Config) ->
    Ids = lists:seq(1,3),
    Requests = [jsonrpc2_test_utils:build_request(<<"sum">>, [1,2,3], Id) || Id <- Ids],
    Responses = jsonrpc2_test_utils:do_json_rpc(Requests),
    _ = jsonrpc2_test_utils:validate_response(Responses, Ids),
    lists:foreach(fun(Response) -> ?assertEqual(6, maps:get(<<"result">>, Response)) end, Responses),
    ok.

invalid_json(_Config) ->
    lists:map(fun(Fun) ->
        Response = jsonrpc2_test_utils:Fun(<<"abcd efgh">>),
        _ = jsonrpc2_test_utils:validate_response(Response, null),
        _ = jsonrpc2_test_utils:validate_error_code(Response, -32700),
        ok
    end, [do_json_rpc, do_json_rpc_with_opts]).
    

invalid_method(_Config) ->
    lists:map(fun(Fun) ->
        Request = jsonrpc2_test_utils:build_request(<<"undefined_method">>, 123, 1),
        Response = jsonrpc2_test_utils:Fun(Request),
        _ = jsonrpc2_test_utils:validate_response(Response, 1),
        _ = jsonrpc2_test_utils:validate_error_code(Response, -32601),
        ok
    end, [do_json_rpc, do_json_rpc_with_opts]).

invalid_params(_Config) ->
    lists:map(fun(Fun) ->
        Request = jsonrpc2_test_utils:build_request(<<"invalid_params">>, 123, 1),
        Response = jsonrpc2_test_utils:Fun(Request),
        _ = jsonrpc2_test_utils:validate_response(Response, 1),
        _ = jsonrpc2_test_utils:validate_error_code(Response, -32602),
        ok
    end, [do_json_rpc, do_json_rpc_with_opts]).

server_error(_Config) ->
    lists:map(fun(Fun) ->
        Request = jsonrpc2_test_utils:build_request(<<"sum">>, [a, b, c], 1),
        Response = jsonrpc2_test_utils:Fun(Request), 
        _ = jsonrpc2_test_utils:validate_response(Response, 1),
        _ = jsonrpc2_test_utils:validate_error_code(Response, -32000),
        ok
    end, [do_json_rpc, do_json_rpc_with_opts]).

batch_request_with_opts(_Config) ->
    Ids = lists:duplicate(3,2),
    Requests = [jsonrpc2_test_utils:build_request(<<"sum">>, [1,2,3], Id) || Id <- Ids],
    Responses = jsonrpc2_test_utils:do_json_rpc_with_opts(Requests),
    _ = jsonrpc2_test_utils:validate_response(Responses, Ids),
    lists:foreach(fun(Response) -> ?assertEqual(12, maps:get(<<"result">>, Response)) end, Responses),
    ok.
