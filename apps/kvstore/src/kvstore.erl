-module(kvstore).
-author('manuel@altenwald.com').

% API
-export([
    init/0,
    stop/0,
    get/1,
    set/2,
    remove/1
]).

%%====================================================================
%% API
%%====================================================================

init() ->
    ets:new(?MODULE, [
        set, named_table, public,
        {read_concurrency, true}
    ]).

stop() ->
    ets:delete(?MODULE).

get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value}] -> Value;
        _ -> undefined
    end.

set(Key, Value) ->
    ets:insert(?MODULE, [{Key, Value}]).

remove(Key) ->
    ets:delete(?MODULE, Key).

%%====================================================================
%% Test functions
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    catch stop(), % ensure always is stopped at begining
    ?assertEqual(undefined, ets:info(?MODULE)),
    kvstore:init(),
    ?assertNotEqual(undefined, ets:info(?MODULE)),
    kvstore:stop(),
    ?assertEqual(undefined, ets:info(?MODULE)),
    ok.

get_set_and_remove_test() ->
    init(),
    Data = [ {A,A*A} || A <- lists:seq(1, 100) ],
    lists:foreach(fun({K,V}) ->
        kvstore:set(K, V)
    end, Data),
    Shuffle = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Data])],
    lists:foreach(fun({K,V}) ->
        ?assertEqual(V, kvstore:get(K)),
        kvstore:remove(K),
        ?assertEqual(undefined, kvstore:get(K))
    end, Shuffle),
    stop(),
    ok.

-endif.
