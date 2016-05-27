-module(kv_http).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start/0,
    stop/1,
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(state, {headers = [], length, method, content, uri, socket, csocket}).

%%====================================================================
%% API
%%====================================================================

start() ->
    case application:get_env(kvstore, http, []) of
        [] ->
            lager:info("HTTP disabled~n", []),
            [];
        #{ port := Port }=Cfg ->
            {ok, Socket} = gen_tcp:listen(Port, [
                binary, inet,
                {active, false},
                {reuseaddr, true},
                {packet, http}
            ]),
            case maps:find(poolsize, Cfg) of
                {ok, Poolsize} -> ok;
                _ -> Poolsize = 10
            end,
            lists:map(fun(N) ->
                {
                    "kvstore_http_" ++ integer_to_list(N),
                    {?MODULE, start_link, [Socket]},
                    permanent, brutal_kill, worker, [?MODULE]
                }
            end, lists:seq(1, Poolsize));
        _ ->
            lager:error("port not defined in HTTP configuration!", []),
            throw({error, enoport})
    end.

stop(PID) ->
    gen_server:call(PID, stop).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Socket]) ->
    gen_server:cast(self(), wait),
    {ok, #state{socket=Socket}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(process, #state{csocket=Socket, method='GET', uri=KeyStr}=State) ->
    <<"/", Key/binary>> = list_to_binary(KeyStr),
    case kvstore:get(Key) of
        undefined ->
            Msg = "HTTP/1.1 404 Not Found\r\ncontent-length: 0\r\n\r\n";
        Value when is_binary(Value) ->
            lager:info("get [~p] => ~p", [Key, Value]),
            Msg = "HTTP/1.1 200 OK\r\ncontent-type: text/plain\r\n" ++
                  "content-length: " ++ integer_to_list(byte_size(Value)) ++
                  "\r\n\r\n" ++ binary_to_list(Value)
    end,
    gen_tcp:send(Socket, Msg),
    inet:setopts(Socket, [{active,once}, {packet,http}]),
    {noreply, State};

handle_cast(process, #state{csocket=Socket, method='DELETE', uri=KeyStr}=State) ->
    <<"/", Key/binary>> = list_to_binary(KeyStr),
    kvstore:remove(Key),
    lager:info("removed [~p]", [Key]),
    gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"),
    inet:setopts(Socket, [{active,once}, {packet,http}]),
    {noreply, State};

handle_cast(process, #state{csocket=Socket, method=M, uri=KeyStr}=State)
        when M =:= 'PUT' orelse M =:= 'POST' ->
    <<"/", Key/binary>> = list_to_binary(KeyStr),
    Value = State#state.content,
    kvstore:set(Key, Value),
    lager:info("setted [~p] => ~p", [Key, Value]),
    gen_tcp:send(Socket, "HTTP/1.1 200 OK\r\ncontent-length: 0\r\n\r\n"),
    inet:setopts(Socket, [{active,once}, {packet,http}]),
    {noreply, State};

handle_cast(process, #state{csocket=Socket, method=_Command}=State) ->
    lager:warning("unknown command: ~p", [_Command]),
    gen_tcp:send(Socket, "HTTP/1.1 405 Method Not Allowed\r\n"
                         "content-length: 0\r\n\r\n"),
    inet:setopts(Socket, [{active,once}, {packet,http}]),
    {noreply, State};

handle_cast(wait, #state{socket=Socket}=State) ->
    case gen_tcp:accept(Socket, 200) of
        {ok, ClientSocket} ->
            lager:info("accepted ~p", [self()]),
            inet:setopts(ClientSocket, [{active,once}]),
            {noreply, State};
        {error, timeout} ->
            gen_server:cast(self(), wait),
            {noreply, State};
        _ ->
            {stop, normal, State}
    end.

handle_info({tcp_closed, Socket}, State) ->
    gen_server:cast(self(), wait),
    gen_tcp:close(Socket),
    {noreply, State};

handle_info({http, Socket, {http_request, Method, {abs_path, URI}, _}}, State) ->
    inet:setopts(Socket, [{active,once}]),
    lager:debug("get request: ~p ~p", [Method, URI]),
    {noreply, State#state{method = Method, csocket = Socket, content = <<>>,
                          uri = URI, headers = [], length = 0}};

handle_info({http, Socket, {http_header,_,Name,_,Value}}, #state{headers=H}=State) ->
    UName = if
        is_atom(Name) -> string:to_lower(atom_to_list(Name));
        is_list(Name) -> string:to_lower(Name)
    end,
    case UName of
        "content-length" ->
            Length = list_to_integer(Value),
            NState = State#state{length=Length};
        _ ->
            NState = State
    end,
    inet:setopts(Socket, [{active,once}]),
    lager:debug("get header: ~p: ~p", [UName,Value]),
    lager:debug("state: ~p", [NState#state{headers=[{UName,Value}|H]}]),
    {noreply, NState#state{headers=[{UName,Value}|H]}};

handle_info({http, Socket, http_eoh}, State) ->
    case State#state.length =:= 0 of
        true ->
            gen_server:cast(self(), process);
        false ->
            inet:setopts(Socket, [{active,once}, {packet,raw}]),
            lager:debug("all the headers gathered: ~p", [State#state.headers])
    end,
    {noreply, State#state{content = <<>>}};

handle_info({tcp, _Socket, Msg}, State) ->
    gen_server:cast(self(), process),
    {noreply, State#state{content=Msg}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _Socket) ->
    ok.

%%====================================================================
%% Test functions
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5555).

config_test() ->
    application:set_env(kvstore, http, #{ port => ?PORT, poolsize => 2 }),
    Data = [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_http:start(),
    ?assertMatch(
        [{
            "kvstore_http_1", {?MODULE, start_link, [Socket]},
            permanent, brutal_kill, worker, [?MODULE]
        },{
            "kvstore_http_2", {?MODULE, start_link, [Socket]},
            permanent, brutal_kill, worker, [?MODULE]
        }],
        Data
    ),
    gen_tcp:close(Socket),
    ok.

config_default_test() ->
    application:set_env(kvstore, http, #{ port => ?PORT }),
    Data = [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_http:start(),
    ?assertEqual(10, length(Data)),
    gen_tcp:close(Socket),
    ok.

config_disabled_test() ->
    application:set_env(kvstore, http, []),
    ?assertEqual([], kv_http:start()),
    ok.

config_exception_test() ->
    application:set_env(kvstore, http, #{}),
    ?assertThrow({error, enoport}, kv_http:start()),
    ok.

close_socket_test() ->
    catch kvstore:init(),
    application:set_env(kvstore, http, #{ port => ?PORT, poolsize => 1 }),
    [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_http:start(),
    {ok, PID} = start_link(Socket),
    gen_tcp:close(Socket),
    timer:sleep(250),
    ?assertEqual(false, is_process_alive(PID)),
    catch kvstore:stop(),
    timer:sleep(100),
    ok.

setup_test_() ->
    {foreach,
        fun() ->
            catch kvstore:init(),
            application:set_env(kvstore, http, #{ port => ?PORT, poolsize => 1 }),
            [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_http:start(),
            {ok, PID} = start_link(Socket),
            {PID, Socket}
        end,
        fun({PID, Socket}) ->
            kv_http:stop(PID),
            gen_tcp:close(Socket),
            catch kvstore:stop(),
            timer:sleep(250)
        end,
        [
            fun get_null/1,
            fun set_and_get/1,
            fun set_and_remove/1,
            fun unknown_command/1,
            fun code_change/1,
            fun close_client/1
        ]
    }.

close_client(_) ->
    ?_test(begin
        {ok, Socket} = gen_tcp:connect({127,0,0,1}, ?PORT,
                                       [binary, {active, false}, {packet, raw}]),
        gen_tcp:close(Socket),
        ok
    end).

http(Method, Key) ->
    URI = "http://127.0.0.1:5555/" ++ Key,
    HOpts = [{timeout, 5000}],
    {ok, {{"HTTP/1.1",Code,_}, _ReturnHeaders, Body}} =
        httpc:request(Method, {URI, []}, HOpts, []),
    {Code, Body}.

http(Method, Key, Value) ->
    URI = "http://127.0.0.1:5555/" ++ Key,
    Headers = [{"content-length", integer_to_list(length(Value))}],
    HOpts = [{timeout, 5000}],
    {ok, {{"HTTP/1.1",Code,_}, _ReturnHeaders, Body}} =
        httpc:request(Method, {URI, Headers, "text/plain", Value}, HOpts, []),
    {Code, Body}.

get_null(_) ->
    ?_test(begin
        ?assertEqual({404,""}, http(get, "hello"))
    end).

set_and_get(_) ->
    ?_test(begin
        ?assertEqual({200,""}, http(put, "hello", "hi")),
        ?assertEqual({200,"hi"}, http(get, "hello"))
    end).

set_and_remove(_) ->
    ?_test(begin
        ?assertEqual({200,""}, http(put, "hello", "hi")),
        ?assertEqual({200,""}, http(delete, "hello")),
        ?assertEqual({404,""}, http(get, "hello"))
    end).

unknown_command(_) ->
    ?_test(begin
        ?assertEqual({405,""}, http(options, ""))
    end).

code_change({PID,_}) ->
    ?_test(begin
        ok = sys:suspend(PID),
        ok = sys:change_code(PID, ?MODULE, "0", []),
        ok = sys:resume(PID)
    end).

-endif.
