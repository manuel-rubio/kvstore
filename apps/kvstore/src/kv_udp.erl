-module(kv_udp).
-author('manuel@altenwald.com').

-behaviour(gen_server).

-export([
    start/0,
    start_link/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(udp, {socket, ip, port, message}).

%%====================================================================
%% API
%%====================================================================

start() ->
    case application:get_env(kvstore, udp, []) of
        [] ->
            lager:info("UDP disabled~n", []),
            [];
        #{ port := Port } ->
            [{
                kv_udp_sup, {?MODULE, start_link, [Port]},
                permanent, brutal_kill, worker, [?MODULE]
            }];
        _ ->
            lager:error("port not defined in UDP configuration!", []),
            throw({error, enoport})
    end.

stop() ->
    gen_server:cast(?MODULE, stop).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port]) ->
    gen_udp:open(Port, [
        binary, inet,
        {active, once},
        {reuseaddr, true}
    ]).

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call(_Msg, _From, State) ->
    lager:warning("request unknown (~p): ~p~n", [_From, _Msg]),
    {reply, ok, State}.

handle_info(#udp{ip=IP, port=Port, message=Info, socket=Socket}, Socket) ->
    case kv_utils:binary_split(Info, <<" ">>) of
        [<<"PUT">>,Key|RawValues] ->
            Value = kv_utils:binary_join(RawValues, <<" ">>),
            kvstore:set(Key, Value),
            lager:info("setted [~p] => ~p", [Key, Value]),
            gen_udp:send(Socket, IP, Port, <<"OK\n">>);
        [<<"GET">>,Key] ->
            case kvstore:get(Key) of
                undefined -> Value = <<"null">>;
                Value when is_binary(Value) -> ok
            end,
            lager:info("get [~p] => ~p", [Key, Value]),
            gen_udp:send(Socket, IP, Port, <<Value/binary, "\n">>);
        [<<"DELETE">>,Key] ->
            kvstore:remove(Key),
            lager:info("removed [~p]", [Key]),
            gen_udp:send(Socket, IP, Port, <<"OK\n">>);
        _Command ->
            lager:warning("unknown command: ~p", [_Command]),
            gen_udp:send(Socket, IP, Port, <<"Unknown command\n">>)
    end,
    inet:setopts(Socket, [{active,once}]),
    {noreply, Socket}.

code_change(_OldVsn, State, _Extra) ->
    lager:info("code changed!"),
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Test functions
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(PORT, 5555).

config_test() ->
    application:set_env(kvstore, udp, #{ port => ?PORT }),
    ?assertEqual(
        [{
            kv_udp_sup, {?MODULE, start_link, [?PORT]},
            permanent, brutal_kill, worker, [?MODULE]
        }],
        kv_udp:start()
    ),
    ok.

config_disabled_test() ->
    application:set_env(kvstore, udp, []),
    ?assertEqual([], kv_udp:start()),
    ok.

config_exception_test() ->
    application:set_env(kvstore, udp, #{}),
    ?assertThrow({error, enoport}, kv_udp:start()),
    ok.

setup_test_() ->
    {foreach,
        fun() ->
            catch kvstore:init(),
            start_link(?PORT),
            ok
        end,
        fun(_) ->
            stop(),
            catch kvstore:stop(),
            timer:sleep(250)
        end,
        [
            fun get_null/1,
            fun set_and_get/1,
            fun set_and_remove/1,
            fun unknown_command/1,
            fun sending_call/1,
            fun code_change/1
        ]
    }.

get_null(_) ->
    ?_test(begin
        {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"GET hello\n">>),
        ?assertMatch({ok, {_,_,<<"null\n">>}}, gen_udp:recv(Socket, 1024)),
        gen_udp:close(Socket),
        ok
    end).

set_and_get(_) ->
    ?_test(begin
        {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"PUT hello hi\n">>),
        ?assertMatch({ok, {_,_,<<"OK\n">>}}, gen_udp:recv(Socket, 1024)),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"GET hello\n">>),
        ?assertMatch({ok, {_,_,<<"hi\n">>}}, gen_udp:recv(Socket, 1024)),
        gen_udp:close(Socket),
        ok
    end).

set_and_remove(_) ->
    ?_test(begin
        {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"PUT hello hi\n">>),
        ?assertMatch({ok, {_,_,<<"OK\n">>}}, gen_udp:recv(Socket, 1024)),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"DELETE hello\n">>),
        ?assertMatch({ok, {_,_,<<"OK\n">>}}, gen_udp:recv(Socket, 1024)),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"GET hello\n">>),
        ?assertMatch({ok, {_,_,<<"null\n">>}}, gen_udp:recv(Socket, 1024)),
        gen_udp:close(Socket),
        ok
    end).

unknown_command(_) ->
    ?_test(begin
        {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
        gen_udp:send(Socket, {127,0,0,1}, ?PORT, <<"HELLO\n">>),
        ?assertMatch({ok, {_,_,<<"Unknown command\n">>}},
                     gen_udp:recv(Socket, 1024)),
        gen_udp:close(Socket),
        ok
    end).

sending_call(_) ->
    ?_assertEqual(ok, gen_server:call(?MODULE, whatever)).

code_change(_) ->
    ?_test(begin
        ok = sys:suspend(?MODULE),
        ok = sys:change_code(?MODULE, ?MODULE, "0", []),
        ok = sys:resume(?MODULE)
    end).

-endif.
