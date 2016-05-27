-module(kvstore_app).
-author('manuel@altenwald.com').

-behaviour(application).
-behaviour(supervisor).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init(Children) ->
    kvstore:init(),
    {ok, {{one_for_one, 100, 1}, Children}}.


%%====================================================================
%% Application API
%%====================================================================

start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
        kv_tcp:start() ++
        kv_udp:start() ++
        kv_http:start()
    ).

%%--------------------------------------------------------------------
stop(_State) ->
    kvstore:stop(),
    ok.

%%====================================================================
%% Test functions
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    ?assertEqual({ok, {{one_for_one, 100, 1}, []}}, init([])),
    kvstore:stop().

start_test() ->
    application:set_env(kvstore, udp, []),
    application:set_env(kvstore, tcp, []),
    application:set_env(kvstore, http, []),
    kvstore_app:start(normal, []),
    ?assertEqual([], supervisor:which_children(kvstore_app)),
    kvstore_app:stop([]).

-endif.
