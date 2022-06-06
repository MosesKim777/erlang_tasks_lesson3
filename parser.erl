%%%-------------------------------------------------------------------
%%% @author apple
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. февр. 2022 15:41
%%%-------------------------------------------------------------------
-module(parser).
-author("apple").
%% API
-export([decode/1, delete_unnecessary_elements/2]).

decode(Data) ->
  Data1 = delete_unnecessary_elements(Data, <<>>),
  case get_type(Data1) of
    object ->
      parse_object(Data1, []);
    array ->
      parse_array(Data1, []);
   _ -> error
  end.

delete_unnecessary_elements(<<>>, Acc) ->
  Acc;
delete_unnecessary_elements(<<"\"", Rest/binary>>, Acc) ->
  get_string(Rest, <<Acc/binary,"\"">>);
delete_unnecessary_elements(<<" ", Rest/binary>>, Acc) ->
  delete_unnecessary_elements(Rest, Acc);
delete_unnecessary_elements(<<"\n", Rest/binary>>, Acc) ->
  delete_unnecessary_elements(Rest, Acc);
delete_unnecessary_elements(<<X, Rest/binary>>, Acc) ->
  delete_unnecessary_elements(Rest, <<Acc/binary,X>>).

get_string(<<"\"", Rest/binary>>, Acc) ->
  delete_unnecessary_elements(Rest, <<Acc/binary,"\"">>);
get_string(<<X, Rest/binary>>, Acc) ->
  get_string(Rest, <<Acc/binary,X>>).

parse_object(<<"{", Rest/binary>>, Acc) ->
  parse_object(Rest, Acc);
parse_object(<<"}">>, Acc) ->
  lists:reverse(Acc);
parse_object(<<",", Rest/binary>>, Acc) ->
  parse_object(Rest, Acc);
parse_object(<<"}", Rest/binary>>, Acc) ->
  {lists:reverse(Acc), Rest};
parse_object(Data, Acc) ->
  {Key, Rest} = get_key(Data, <<>>),
  {Value, Rest1} = get_value(Rest),
  parse_object(Rest1, [{Key,Value}|Acc]).


get_key(<<"\"", Rest/binary>>, Acc) ->
  get_key(Rest, Acc);
get_key(<<":", Rest/binary>>, Acc) ->
  {binary_to_list(Acc), Rest};
get_key(<<X, Rest/binary>>, Acc) ->
  get_key(Rest, <<Acc/binary, X>>).

get_value(Data) ->
  case get_type(Data) of
    string ->
      parse_string(Data, <<>>);
    number ->
      parse_number(Data, <<>>);
    object ->
      parse_object(Data, []);
    array ->
      parse_array(Data, []);
    atom ->
      parse_atom(Data, <<>>)
  end.


parse_array(<<"[",Rest/binary>>, []) ->
  parse_array(Rest, []);
parse_array(<<",",Rest/binary>>, Acc) ->
  parse_array(Rest, Acc);
parse_array(<<"]">>, Acc) ->
  lists:reverse(Acc);
parse_array(<<"]", Rest/binary>>, Acc) ->
  {lists:reverse(Acc), Rest};
parse_array(Data, Acc) ->
  {El,Rest} =  case get_type(Data) of
                 string ->
                   parse_string(Data, <<>>);
                 number ->
                   parse_number(Data, <<>>);
                 object ->
                   parse_object(Data, []);
                 array ->
                   parse_array(Data, []);
                 atom ->
                   parse_atom(Data, <<>>)
               end,
  parse_array(Rest, [El|Acc]).

parse_string(<<X, Rest/binary>> = Data, Acc) ->
  case <<X/utf8>> of
    <<"\"">> -> parse_string(Rest, Acc);
    <<",">> -> {binary_to_list(Acc),Rest};
    <<"}">> -> {binary_to_list(Acc), Data};
    <<"]">> -> {binary_to_list(Acc), Data};
    _ -> parse_string(Rest, <<Acc/binary, X>>)
  end.

parse_atom(<<"}", _Rest/binary>> = Data, Acc) ->
  {binary_to_atom(Acc, utf8), Data};
parse_atom(<<"]", _Rest/binary>> = Data, Acc) ->
  {binary_to_atom(Acc, utf8), Data};
parse_atom(<<",", Rest/binary>>, Acc) ->
  {binary_to_atom(Acc, utf8), Rest};
parse_atom(<<X, Rest/binary>>, Acc) ->
  parse_atom(Rest, <<Acc/binary,X>>).

parse_number(<<".", Rest/binary>>, Acc) ->
  parse_number_float(Rest, <<Acc/binary,".">>);
parse_number(<<"}", _Rest/binary>> = Json, Acc) ->
  {binary_to_integer(Acc), Json};
parse_number(<<"]", _Rest/binary>> = Json, Acc) ->
  {binary_to_integer(Acc), Json};
parse_number(<<",", Rest/binary>>, Acc) ->
  {binary_to_integer(Acc), Rest};
parse_number(<<X, Rest/binary>>, Acc) ->
  parse_number(Rest, <<Acc/binary,X>>).

parse_number_float(<<"}", _Rest/binary>> = Json, Acc) ->
  {binary_to_float(Acc), Json};
parse_number_float(<<"]", _Rest/binary>> = Json, Acc) ->
  {binary_to_float(Acc), Json};
parse_number_float(<<",", Rest/binary>>, Acc) ->
  {binary_to_float(Acc), Rest};
parse_number_float(<<X, Rest/binary>>, Acc) ->
  parse_number_float(Rest, <<Acc/binary,X>>).

get_type(<<X, _Rest/binary>>) ->
  case <<X/utf8>> of
    <<",">> -> comma;
    <<"{">> -> object;
    <<"[">> -> array;
    <<"\"">> -> string;
    <<"0">> -> number;
    <<"1">> -> number;
    <<"2">> -> number;
    <<"3">> -> number;
    <<"4">> -> number;
    <<"5">> -> number;
    <<"6">> -> number;
    <<"7">> -> number;
    <<"8">> -> number;
    <<"9">> -> number;
    _ -> atom
  end.