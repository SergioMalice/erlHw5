%%%-------------------------------------------------------------------
%%% @author sergeyb
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Июль 2019 16:51
%%%-------------------------------------------------------------------
-module(speed_test).
-author("sergeyb").
-record(speed, {operation, time, data_type}).

%%
% Запуск теста - speed_test:print()
%
% Для наглядности тестов скорости создадим ETS - таблицу с такими записями:
%
% #speed{operation, data_type, time}, где
%
% operation = create, update, pattern_match_read, function_read, что соответственно означает
% скорость создания, обновления полей, чтения значения через сопоставление по образцу, чтение через вызов функций;
% data_type = map, proplist, dict, process_dict, ets, record;
% ну и time - записанное время выполнения операции.
%
% Тестировать будем вышеуказанные операции на заданных типах данных, на примере -
%
% Создание структуры - записываем ?Count пар Cnt, Cnt
% Изменение - ?Count раз изменяем значение пары со случайным Index ключем, на Index
% Чтение - ?Count раз читаем значение пары со случайным Index ключем
%
% В конце тестов в консоли будут отпечатаны результаты, отсортированные по типу
% операции и времени выполнения.


%% API
-export([print/0, map/0, map_create/2, map_update/2, map_pattern/2, map_function/2]).
-export([proplist/0, proplist_create/2, proplist_update/2, proplist_pattern/2, proplist_function/2]).
-export([dict/0, dict_create/2, dict_update/2, dict_function/2]).
-export([process_dict/0, process_dict_create/1, process_dict_update/1, process_dict_function/2]).
-export([ets/0, ets_create/1, ets_update/1, ets_function/1]).
-export([record/0, record_create/2, record_update/2, record_pattern/2]).

-record(pair, {key, value}).
-define(Count, 10000).

print() ->
  ets:new(table, [named_table, duplicate_bag, {keypos, #speed.operation}]),
  map(), proplist(), dict(), process_dict(), ets(), record(),
  io:format("Create speed: ~p~n~n", [lists:sort(ets:lookup(table, create))]),
  io:format("Update speed: ~p~n~n", [lists:sort(ets:lookup(table, update))]),
  io:format("Pattern match read speed: ~p~n~n", [lists:sort(ets:lookup(table, pattern_match_read))]),
  io:format("Function read speed: ~p~n~n", [lists:sort(ets:lookup(table, function_read))]).

% Тестирование maps

map() ->
  {Create, Map} = timer:tc(speed_test, map_create, [0, #{}]),
  ets:insert(table, #speed{operation = create, data_type = map, time = Create}),
  {Update, _} = timer:tc(speed_test, map_update, [0, Map]),
  ets:insert(table, #speed{operation = update, data_type = map, time = Update}),
  {Pattern, _} = timer:tc(speed_test, map_pattern, [0, Map]),
  ets:insert(table, #speed{operation = pattern_match_read, data_type = map, time = Pattern}),
  {Function, _} = timer:tc(speed_test, map_function, [0, Map]),
  ets:insert(table, #speed{operation = function_read, data_type = map, time = Function}).

map_create(?Count, Res) -> Res;
map_create(Cnt, Res) -> map_create(Cnt+1, maps:put(Cnt, Cnt, Res)).

map_update(?Count, Res) -> Res;
map_update(Cnt, Map) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  map_update(Cnt+1, maps:update(Index, Index, Map)).

map_pattern(?Count, Res) -> Res;
map_pattern(Cnt, Map) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  #{Index := Value} = Map,
  Value, map_pattern(Cnt+1, Map).

map_function(?Count, Res) -> Res;
map_function(Cnt, Map) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  maps:get(Index, Map),
  map_function(Cnt+1, Map).

% Тестирование списков

proplist() ->
  {Create, List} = timer:tc(speed_test, proplist_create, [0, []]),
  ets:insert(table, #speed{operation = create, data_type = proplist, time = Create}),
  {Update, _} = timer:tc(speed_test, proplist_update, [0, List]),
  ets:insert(table, #speed{operation = update, data_type = proplist, time = Update}),
  {Pattern, _} = timer:tc(speed_test, proplist_pattern, [0, List]),
  ets:insert(table, #speed{operation = pattern_match_read, data_type = proplist, time = Pattern}),
  {Function, _} = timer:tc(speed_test, proplist_function, [0, List]),
  ets:insert(table, #speed{operation = function_read, data_type = proplist, time = Function}).

proplist_create(?Count, Res) -> lists:reverse(Res);
proplist_create(Cnt, List) -> proplist_create(Cnt+1, [{Cnt, Cnt}|List]).

proplist_update(?Count, Res) -> Res;
proplist_update(Cnt, List) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  proplist_update(Cnt+1, lists:keyreplace(Index, 1, List, {Index, Index})).

proplist_pattern_lookup([{Key, Value}|_], Key) -> Value;
proplist_pattern_lookup([{_, _}|T], Key) -> proplist_pattern_lookup(T, Key).

proplist_pattern(?Count, Res) -> Res;
proplist_pattern(Cnt, List) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  proplist_pattern_lookup(List, Index),
  proplist_pattern(Cnt+1, List).

proplist_function(?Count, Res) -> Res;
proplist_function(Cnt, List) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  proplists:get_value(Index, List),
  proplist_function(Cnt+1, List).

% Тестирование dict

dict() ->
  {Create, Dict} = timer:tc(speed_test, dict_create, [0, dict:new()]),
  ets:insert(table, #speed{operation = create, data_type = dict, time = Create}),
  {Update, _} = timer:tc(speed_test, dict_update, [0, Dict]),
  ets:insert(table, #speed{operation = update, data_type = dict, time = Update}),
  {Function, _} = timer:tc(speed_test, dict_function, [0, Dict]),
  ets:insert(table, #speed{operation = function_read, data_type = dict, time = Function}).

dict_create(?Count, Res) -> Res;
dict_create(Cnt, Dict) -> dict_create(Cnt+1, dict:store(Cnt, Cnt, Dict)).

dict_update(?Count, Res) -> Res;
dict_update(Cnt, Dict) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  dict_update(Cnt+1, dict:update(Index, fun(_) -> Index end, Dict)).

dict_function(?Count, Res) -> Res;
dict_function(Cnt, Dict) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  dict:fetch(Index, Dict),
  dict_function(Cnt+1, Dict).

% Тестирование process dictionary

process_dict() ->
  {Create, _} = timer:tc(speed_test, process_dict_create, [0]),
  ets:insert(table, #speed{operation = create, data_type = process_dict, time = Create}),
  {Update, _} = timer:tc(speed_test, process_dict_update, [0]),
  ets:insert(table, #speed{operation = update, data_type = process_dict, time = Update}),
  {Function, _} = timer:tc(speed_test, process_dict_function, [0, 0]),
  ets:insert(table, #speed{operation = function_read, data_type = process_dict, time = Function}).

process_dict_create(?Count) -> ok;
process_dict_create(Cnt) -> put(Cnt, Cnt).

process_dict_update(?Count) -> ok;
process_dict_update(Cnt) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  put(Index, Index),
  process_dict_update(Cnt+1).

process_dict_function(?Count, Res) -> Res;
process_dict_function(Cnt, _) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  process_dict_function(Cnt+1, get(Index)).

% Тестирование ETS

ets() ->
  ets:new(test, [named_table]),
  {Create, _} = timer:tc(speed_test, ets_create, [0]),
  ets:insert(table, #speed{operation = create, data_type = ets, time = Create}),
  {Update, _} = timer:tc(speed_test, ets_update, [0]),
  ets:insert(table, #speed{operation = update, data_type = ets, time = Update}),
  {Function, _} = timer:tc(speed_test, ets_function, [0]),
  ets:insert(table, #speed{operation = function_read, data_type = ets, time = Function}).

ets_create(?Count) -> ok;
ets_create(Cnt) ->
  ets:insert(test, {Cnt, Cnt}),
  ets_create(Cnt+1).

ets_update(?Count) -> ok;
ets_update(Cnt) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  ets:insert(test, {Index, Index}),
  ets_update(Cnt+1).

ets_function(?Count) -> ok;
ets_function(Cnt) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  ets:lookup(test, Index),
  ets_function(Cnt+1).

% Тестирование records

record() ->
  {Create, Rec} = timer:tc(speed_test, record_create, [0, []]),
  ets:insert(table, #speed{operation = create, data_type = record, time = Create}),
  {Update, _} = timer:tc(speed_test, record_update, [0, Rec]),
  ets:insert(table, #speed{operation = update, data_type = record, time = Update}),
  {Pattern, _} = timer:tc(speed_test, record_pattern, [0, Rec]),
  ets:insert(table, #speed{operation = pattern_match_read, data_type = record, time = Pattern}).

record_create(?Count, Res) -> lists:reverse(Res);
record_create(Cnt, Res) -> record_create(Cnt+1, [#pair{key = Cnt, value = Cnt}|Res]).

record_update(?Count, Res) -> Res;
record_update(Cnt, Rec) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  record_update(Cnt+1, lists:keyreplace(Index, 2, Rec, #pair{value = Index})).

record_pattern_lookup([#pair{key = Key, value = Value}|_], Key) -> Value;
record_pattern_lookup([_|T], Key) -> record_pattern_lookup(T, Key).

record_pattern(?Count, Res) -> Res;
record_pattern(Cnt, Rec) ->
  Index = round(rand:uniform()*(?Count-2))+1,
  record_pattern_lookup(Rec, Index),
  record_pattern(Cnt+1, Rec).