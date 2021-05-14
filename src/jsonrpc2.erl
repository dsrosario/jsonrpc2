-module(jsonrpc2).

%% API exports
-export([handle/2]).

-ignore_xref([{handle, 2}]).

%% Type definitions
-type rpc_error_reason() :: parse_error |
                            invalid_request |
                            method_not_found |
                            invalid_params |
                            internal_error |
                            server_error |
                            {integer(), binary()}.

-type rpc_handler_fun() :: fun((binary(), jsx:json_term(), jsx:json_term() | undefined) -> {ok, jsx:json_term()} | 
                                                                                           {error, rpc_error_reason()} |
                                                                                           {error, {rpc_error_reason(), jsx:json_term()}}).
-type rpc_id() :: null | binary() | number().

-export_type([rpc_error_reason/0]).

%%====================================================================
%% API functions
%%====================================================================
-spec handle(Data :: jsx:json_text(),
             Handler :: rpc_handler_fun()) -> {reply, jsx:json_text()} | noreply.
handle(Data, Handler) when is_binary(Data) andalso is_function(Handler, 3) ->
    Response = 
        try 
            begin
                Request = jsx:decode(Data, [return_maps]),
                dispatch_rpc(Request, Handler)
            end
        catch
            C:E -> 
                _ = lager:debug("parse error: ~p", [{C,E}]),
                make_error_response(parse_error, undefined, null)
        end,
    case Response of
        noreply -> 
            _ = lager:debug("no reply"),
            noreply;
        {reply, Reply} -> 
            _ = lager:debug("reply ~p", [Reply]),
            {reply, jsx:encode(Reply)}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
-spec dispatch_rpc(Request :: jsx:json_term(),
                   Handler :: rpc_handler_fun()) -> {reply, jsx:json_term()} | noreply.
dispatch_rpc(Request, Handler) when is_map(Request) ->
    process_rpc_data(Request, Handler);
dispatch_rpc(Requests, Handler) when is_list(Requests) ->
    Replies = lists:filtermap(
        fun(Request) -> 
            case process_rpc_data(Request, Handler) of
                {reply, Reply} -> {true, Reply};
                noreply -> false
            end
        end,
        Requests),
    case Replies of
        [] -> noreply;
        _ -> {reply, Replies}
    end.

-spec process_rpc_data(Request :: jsx:json_term(),
                       Handler :: rpc_handler_fun()) -> {reply, jsx:json_term()} | noreply.
process_rpc_data(#{<<"jsonrpc">> := <<"2.0">>,
                   <<"method">> := Method,
                   <<"params">> := Params} = Request, Handler) ->
    Id = maps:get(<<"id">>, Request, undefined),
    execute_rpc(Method, Params, Id, Handler);
process_rpc_data(Request, _) ->
    _ = lager:debug("Invalid request: ~p", [Request]),
    make_error_response(invalid_request, undefined, null).

-spec execute_rpc(Method :: binary(),
                  Params :: jsx:json_term(),
                  Id :: jsx:json_term() | undefined,
                  Handler :: rpc_handler_fun()) -> {reply, jsx:json_term()} | noreply.
execute_rpc(Method, Params, Id, Handler)
    when Id =:= undefined orelse
         Id =:= null orelse
         is_binary(Id) orelse
         is_number(Id) ->
    try 
        case Handler(Method, Params, Id) of
            {ok, Result} -> 
                make_result_response(Result, Id);
            {error, {Reason, ErrorData}} ->
                make_error_response(Reason, ErrorData, Id);
            {error, Reason} ->
                make_error_response(Reason, undefined, Id)
        end
    catch
        error:function_clause ->
            make_error_response(method_not_found, undefined, Id);
        C:E ->
            _ = lager:error("Server Error: ~p", [{C,E}]),
            make_error_response(server_error, undefined, Id)
    end;
execute_rpc(_, _, _, _) ->
    make_error_response(invalid_request, undefined, undefined).

-spec get_error_code_and_message(Reason :: rpc_error_reason()) -> {integer(), binary()}.
get_error_code_and_message(parse_error) -> {-32700, <<"Parse error">>};
get_error_code_and_message(invalid_request) -> {-32600, <<"Invalid Request">>};
get_error_code_and_message(method_not_found) -> {-32601, <<"Method not found">>};
get_error_code_and_message(invalid_params) -> {-32602, <<"Invalid params">>};
get_error_code_and_message(internal_error) -> {-32603, <<"Internal error">>};
get_error_code_and_message(server_error) -> {-32000, <<"Server error">>};
get_error_code_and_message({Code, Message}) when is_integer(Code) andalso is_binary(Message) -> {Code, Message};
get_error_code_and_message(_) -> {-32001, <<"Unknown Server error">>}.

-spec make_error_response(Reason :: rpc_error_reason(),
                          Data :: undefined | jsx:json_term(),
                          Id :: rpc_id() | undefined) -> noreply | {reply, jsx:json_term()}.
make_error_response(Reason, Data, Id) ->
    {Code, Message} = get_error_code_and_message(Reason),
    make_error_response(Code, Message, Data, Id).

-spec make_error_response(Code :: integer(),
                          Message :: binary(),
                          Data :: undefined | jsx:json_term(),
                          Id :: rpc_id() | undefined) -> noreply | {reply, jsx:json_term()}.
make_error_response(_, _, _, undefined) ->
    noreply;
make_error_response(Code, Message, Data, Id) ->
    ErrorObject0 = #{code => Code, message => Message},
    ErrorObject = case Data of
                      undefined -> ErrorObject0;
                      _ -> maps:put(data, Data, ErrorObject0)
                  end,  
    {reply, #{jsonrpc => <<"2.0">>,
              error => ErrorObject,
              id => Id}}.

-spec make_result_response(Result :: jsx:json_term(),
                           Id :: rpc_id() | undefined) -> noreply | {reply, jsx:json_term()}.
make_result_response(_, undefined) -> 
    noreply;
make_result_response(Result, Id) ->
    {reply, #{jsonrpc => <<"2.0">>,
              result => Result,
              id => Id}}.
