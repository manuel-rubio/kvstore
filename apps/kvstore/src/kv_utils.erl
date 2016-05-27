-module(kv_utils).
-author('manuel@altenwald.com').

-export([
    binary_join/2,
    binary_split/2,
    rtrim/1
]).

% from: https://coderwall.com/p/nmajna/joining-a-list-of-binaries-in-erlang
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).

binary_split(Subject, Pattern) ->
    [ rtrim(X) || X <- binary:split(Subject, Pattern, [global]) ].

rtrim(<<>>) -> <<>>;
rtrim(Bin) ->
    Size = size(Bin) - 1,
    <<BinHead:Size/binary,C>> = Bin,
    case is_whitespace(C) of
        true -> rtrim(BinHead);
        false -> Bin
    end.

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_) -> false.

%%====================================================================
%% Test functions
%%====================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

rtrim_test() ->
    ?assertEqual(<<"hello">>, rtrim(<<"hello  \r\n\t">>)),
    ?assertEqual(<<"hello">>, rtrim(<<"hello\n">>)),
    ?assertEqual(<<"  hello">>, rtrim(<<"  hello  ">>)),
    ok.

split_test() ->
    ?assertEqual([<<"hello">>, <<"world!">>],
                 binary_split(<<"hello world!">>, <<" ">>)),
    ?assertEqual([<<"hello">>], binary_split(<<"hello">>, <<" ">>)),
    ok.

join_test() ->
    ?assertEqual(<<>>, binary_join([], <<",">>)),
    ?assertEqual(<<"hello">>, binary_join([<<"hello">>], <<",">>)),
    ?assertEqual(<<"1,2">>, binary_join([<<"1">>, <<"2">>], <<",">>)),
    ok.

split_and_join_test() ->
    Strings = [
        <<"hello world!">>,
        <<"this is a string!">>,
        <<"no-espaces-here">>,
        <<>>
    ],
    lists:foreach(fun(S) ->
        ?assertEqual(S, binary_join(binary_split(S, <<" ">>), <<" ">>))
    end, Strings),
    ok.

-endif.
