-module(kv_tcp).
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

%%====================================================================
%% API
%%====================================================================

start() ->
    case application:get_env(kvstore, tcp, []) of
        [] ->
            lager:info("TCP disabled~n", []),
            [];
        #{ port := Port }=Cfg ->
            {ok, Socket} = gen_tcp:listen(Port, [
                binary, inet,
                {active, false},
                {reuseaddr, true},
                {packet, raw}
            ]),
            case maps:find(poolsize, Cfg) of
                {ok, Poolsize} -> ok;
                _ -> Poolsize = 10
            end,
            lists:map(fun(N) ->
                {
                    "kvstore_tcp_" ++ integer_to_list(N),
                    {?MODULE, start_link, [Socket]},
                    permanent, brutal_kill, worker, [?MODULE]
                }
            end, lists:seq(1, Poolsize));
        _ ->
            lager:error("port not defined in TCP configuration!", []),
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
    {ok, Socket}.

handle_call(stop, From, State) ->
    gen_server:reply(From, ok),
    {stop, normal, State}.

handle_cast(wait, Socket) ->
    case gen_tcp:accept(Socket, 200) of
        {ok, ClientSocket} ->
            lager:info("accepted ~p", [self()]),
            inet:setopts(ClientSocket, [{active,once}]),
            {noreply, Socket};
        {error, timeout} ->
            gen_server:cast(self(), wait),
            {noreply, Socket};
        _ ->
            {stop, normal, Socket}
    end.

handle_info({tcp_closed, Socket}, State) ->
    gen_server:cast(self(), wait),
    gen_tcp:close(Socket),
    {noreply, State};

handle_info({tcp, Socket, Info}, State) ->
    case kv_utils:binary_split(Info, <<" ">>) of
        [<<"PUT">>,Key|RawValues] ->
            Value = kv_utils:binary_join(RawValues, <<" ">>),
            kvstore:set(Key, Value),
            lager:info("setted [~p] => ~p", [Key, Value]),
            gen_tcp:send(Socket, <<"OK\n">>);
        [<<"GET">>,Key] ->
            case kvstore:get(Key) of
                undefined -> Value = <<"null">>;
                Value when is_binary(Value) -> ok
            end,
            lager:info("get [~p] => ~p", [Key, Value]),
            gen_tcp:send(Socket, <<Value/binary, "\n">>);
        [<<"DELETE">>,Key] ->
            kvstore:remove(Key),
            lager:info("removed [~p]", [Key]),
            gen_tcp:send(Socket, <<"OK\n">>);
        [<<"QUIT">>|_] ->
            gen_server:cast(self(), wait),
            gen_tcp:close(Socket);
        _Command ->
            lager:warning("unknown command: ~p", [_Command]),
            gen_tcp:send(Socket, <<"Unknown command\n">>)
    end,
    inet:setopts(Socket, [{active,once}]),
    {noreply, State}.

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
    application:set_env(kvstore, tcp, #{ port => ?PORT, poolsize => 2 }),
    Data = [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_tcp:start(),
    ?assertMatch(
        [{
            "kvstore_tcp_1", {?MODULE, start_link, [Socket]},
            permanent, brutal_kill, worker, [?MODULE]
        },{
            "kvstore_tcp_2", {?MODULE, start_link, [Socket]},
            permanent, brutal_kill, worker, [?MODULE]
        }],
        Data
    ),
    gen_tcp:close(Socket),
    ok.

config_default_test() ->
    application:set_env(kvstore, tcp, #{ port => ?PORT }),
    Data = [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_tcp:start(),
    ?assertEqual(10, length(Data)),
    gen_tcp:close(Socket),
    ok.

config_disabled_test() ->
    application:set_env(kvstore, tcp, []),
    ?assertEqual([], kv_tcp:start()),
    ok.

config_exception_test() ->
    application:set_env(kvstore, tcp, #{}),
    ?assertThrow({error, enoport}, kv_tcp:start()),
    ok.

close_socket_test() ->
    catch kvstore:init(),
    application:set_env(kvstore, tcp, #{ port => ?PORT, poolsize => 1 }),
    [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_tcp:start(),
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
            application:set_env(kvstore, tcp, #{ port => ?PORT, poolsize => 1 }),
            [{_,{_,_,[Socket]},_,_,_,_}|_] = kv_tcp:start(),
            {ok, PID} = start_link(Socket),
            {PID, Socket}
        end,
        fun({PID, Socket}) ->
            kv_tcp:stop(PID),
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
            fun quit_command/1
        ]
    }.

connect() ->
    gen_tcp:connect({127,0,0,1}, ?PORT, [binary, {active, false}, {packet, raw}]).

get_null(_) ->
    ?_test(begin
        {ok, Socket} = connect(),
        gen_tcp:send(Socket, <<"GET hello\n">>),
        ?assertMatch({ok, <<"null\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:close(Socket),
        ok
    end).

set_and_get(_) ->
    ?_test(begin
        {ok, Socket} = connect(),
        gen_tcp:send(Socket, <<"PUT hello hi\n">>),
        ?assertMatch({ok, <<"OK\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:send(Socket, <<"GET hello\n">>),
        ?assertMatch({ok, <<"hi\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:close(Socket),
        ok
    end).

set_and_remove(_) ->
    ?_test(begin
        {ok, Socket} = connect(),
        gen_tcp:send(Socket, <<"PUT hello hi\n">>),
        ?assertMatch({ok, <<"OK\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:send(Socket, <<"DELETE hello\n">>),
        ?assertMatch({ok, <<"OK\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:send(Socket, <<"GET hello\n">>),
        ?assertMatch({ok, <<"null\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:close(Socket),
        ok
    end).

unknown_command(_) ->
    ?_test(begin
        {ok, Socket} = connect(),
        gen_tcp:send(Socket, <<"HELLO\n">>),
        ?assertMatch({ok, <<"Unknown command\n">>}, gen_tcp:recv(Socket, 0)),
        gen_tcp:close(Socket),
        ok
    end).

code_change({PID,_}) ->
    ?_test(begin
        ok = sys:suspend(PID),
        ok = sys:change_code(PID, ?MODULE, "0", []),
        ok = sys:resume(PID)
    end).

quit_command(_) ->
    ?_test(begin
        {ok, Socket} = connect(),
        gen_tcp:send(Socket, <<"QUIT\n">>),
        timer:sleep(200),
        gen_tcp:close(Socket),
        ok
    end).

-endif.
