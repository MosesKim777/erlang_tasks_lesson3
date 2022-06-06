%%%-------------------------------------------------------------------
%%% @author apple
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. март 2022 23:28
%%%-------------------------------------------------------------------
-module(task1).
-author("apple").

%% API
-export([first_word/1]).

first_word(BinText) ->
  first_word(BinText, <<>>).

first_word(<<>>, Acc) ->
  Acc;
first_word(<<" ", _Rest/binary>>, Acc) ->
  Acc;
first_word(<<X/utf8, Rest/binary>>, Acc) ->
  first_word(Rest, <<Acc/binary,X/utf8>>).