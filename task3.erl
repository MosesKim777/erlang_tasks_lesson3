%%%-------------------------------------------------------------------
%%% @author apple
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. март 2022 00:00
%%%-------------------------------------------------------------------
-module(task3).
-author("apple").

%% API
-export([split/2]).

len(Val) ->
  len(Val, 0).
len(<<>>, N) ->
  N;
len(<<_H,T/binary>>, N) ->
  len(T, N+1).

split(Bintext, Separ) ->
  split(Bintext, <<>>, 0, list_to_binary(Separ), len(Separ), <<>>, []).

split(Bintext, Separ1, _, Separ, N, Acc, Acc1) when Separ1 == Separ ->
  split(Bintext, <<>>, 0, Separ, N, <<>>, [Acc|Acc1]);
split(Bintext, <<X, Rest/binary>>, N1, Separ, N, Acc, Acc1) when N1 == N ->
  split(Bintext, Rest, N1-1, Separ, N, <<Acc/binary, X>>, Acc1);
split(<<>>, Separ1, _N1, _Separ, _N, Acc, Acc1) ->
  lists:reverse([<<Acc/binary,Separ1/binary>>|Acc1]);
split(<<X, Rest/binary>>, Separ1, N1, Separ, N, Acc, Acc1) ->
  split(Rest, <<Separ1/binary, X>>, N1+1, Separ, N, Acc, Acc1).