%%%-------------------------------------------------------------------
%%% @author apple
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. март 2022 23:44
%%%-------------------------------------------------------------------
-module(task2).
-author("apple").

%% API
-export([words/1]).

words(BinText) ->
  words(BinText, <<>>, []).

words(<<>>, Acc, Acc1) ->
  lists:reverse([Acc|Acc1]);
words(<<" ", Rest/binary>>, Acc, Acc1) ->
  words(Rest, <<>>, [Acc|Acc1]);
words(<<X/utf8, Rest/binary>>, Acc, Acc1) ->
  words(Rest, <<Acc/binary,X/utf8>>, Acc1).