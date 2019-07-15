%%%-------------------------------------------------------------------
%%% @author sergeyb
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Июль 2019 19:24
%%%-------------------------------------------------------------------
-module(my_cache).
-author("sergeyb").

%% API
-include("../include/header.hrl").
-export([create/1, insert/4, lookup/2, delete_obsolete/1]).

% Пример теста для копирования:
% 1) my_cache:create(table).
% 2) my_cache:insert(table, cookie, first, 5).
% 3) my_cache:insert(table, cookie, second, 30).
% 4) my_cache:insert(table, cookie, third, 300).
% 5) my_cache:lookup(table, cookie). (скорее всего, вернет уже только second и third)
% 6) my_cache:delete_obsolete(table).
%
%%
% Модуль my_cache формирует кеш-таблицу ключ-значение с указанием времени жизни пары.
% Описание записи #cache - в подключенном файле header.hrl.
% create/1 создает таблицу с указанным именем, insert/4 - добавляет элемент с временем жизни,
% lookup/2 по заданному ключу возвращает не устаревшие записи,
% delete_obsolete/1 - удаляет устаревшие записи.
%
% В качестве проверки времени жизни используется модуль calendar, для записи количества
% секунд данного момента времени функцией
% calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
%
% Для явного указания ключа записи ets таблице следует передать параметр {keypos, #<имя записи>.<имя поля ключа>}
% Для наглядности использовал тип duplicate_bag, чтобы было видно, как возвращается список актуальных значений
% с одинаковым ключем.
%
% Для удаления методом ets:select_delete матчспека должна возвращать true, иначе объект удален не будет.

create(Name) -> ets:new(Name, [named_table, duplicate_bag, {keypos, #cache.key}]).

insert(TName, Key, Val, Life) ->
  Time = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  ets:insert(TName, #cache{key=Key, value = Val, life = Life, time = Time}).

lookup(TName, Key) ->
  Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  ets:select(TName, [{#cache{key = '$1', value = '$2', life = '$3', time = '$4'},
    [{'>', '$3', {'-', Now, '$4'}}, {'==', '$1', Key}], ['$2']}]).

delete_obsolete(TName) ->
  Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  ets:select_delete(TName, [{#cache{key = '$1', value = '$2', life = '$3', time = '$4'},
    [{'=<', '$3', {'-', Now, '$4'}}], [true]}]).