%%% @hidden
%%% @doc MongoDB store implementation.
%%%
%%% Copyright 2017 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(sumo_store_mongo).
-author("Marcelo Gornstein <marcelog@gmail.com>").
-github("https://github.com/inaka").
-license("Apache License 2.0").

-behavior(sumo_store).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Public API.
-export([
  init/1,
  create_schema/2,
  persist/2,
  fetch/3,
  find_by/3,
  find_by/5,
  find_by/6,
  find_all/2,
  find_all/5,
  delete_by/3,
  delete_all/2,
  count/2,
  count_by/3
]).

%%% WorkerPool API
-export([handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{conn := pid(),
                   ref  := reference()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, term()}.
init(Options) ->
  Backend = proplists:get_value(storage_backend, Options),
  Connection = sumo_backend_mongo:get_connection(Backend),

  Ref = monitor(process, Connection),

  {ok, #{conn => Connection, ref => Ref}}.

-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).
create_schema(Schema, #{conn := Connection} = State) ->
  SchemaName = sumo_internal:schema_name(Schema),
  Fields = sumo_internal:schema_fields(Schema),
  lists:foreach(
    fun(Field) ->
      Name = sumo_internal:field_name(Field),
      ok = mc_worker_api:ensure_index(Connection,
                                      list_to_binary(atom_to_list(SchemaName)),
                                      {list_to_binary(atom_to_list(Name)), 1})
    end,
    Fields
  ),
  {ok, State}.

-spec persist(sumo_internal:doc(), state()) ->
  sumo_store:result(sumo_internal:doc(), state()).
persist(Doc, #{conn := Connection} = State) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Collection = atom_to_binary(DocName),
  {Action, NewId} = case sumo_internal:get_field(IdField, Doc) of
    undefined -> {create, mc_utils:random_nonce(30)};
    Id        -> {update, Id}
  end,

  NewDoc = sumo_internal:set_field(IdField, NewId, Doc),
  NewDoc2 = sleep(NewDoc),
  Fields = sumo_internal:doc_fields(NewDoc2),

  ok = do_persist(Action, Connection, Collection, NewId, Fields),

  {ok, wakeup(NewDoc2), State}.

-spec fetch(DocName, Id, State) -> Response when
  DocName  :: sumo:schema_name(),
  Id       :: sumo:field_value(),
  State    :: state(),
  Response :: sumo_store:result(sumo_internal:doc(), state()).
fetch(DocName, Id, #{conn := Connection} = State) ->
  Collection = atom_to_binary(DocName),
  case mc_worker_api:find_one(Connection, Collection, #{<<"_id">> => Id}) of
    undefined -> {error, notfound, State};
    Result    -> {ok, mmap_to_doc(DocName, Result), State}
  end.

-spec find_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, 0, 0, State).

-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              non_neg_integer(),
              non_neg_integer(),
              state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, Limit, Offset, State) ->
  find_by(DocName, Conditions, [], Limit, Offset, State).

-spec find_by(sumo:schema_name(),
              sumo:conditions(),
              term(),
              non_neg_integer(),
              non_neg_integer(),
              state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName,
        Conditions,
        SortFields,
        Limit,
        Offset,
        #{conn := Connection} = State
       ) ->
  Args = case Offset of
    0 -> #{};
    Offset -> #{batchsize => Limit, skip => Offset}
  end,

  Sort = case SortFields of
           [] -> [];
           _  -> lists:map(fun sort_fields/1, SortFields)
         end,

  TranslatedConditions = transform_conditions(DocName, Conditions),
  Selector = #{<<"$query">> => get_query(TranslatedConditions), <<"$orderby">> => Sort},
  Collection = atom_to_binary(DocName),

  Results =
    case mc_worker_api:find(Connection, Collection, Selector, Args) of
      {ok, Cursor} ->
        R = case map_size(Args) of
          0  -> mc_cursor:rest(Cursor);
          _  -> mc_cursor:next_batch(Cursor)
        end,
        mc_cursor:close(Cursor),
        R;
      [] ->
        []
    end,

  Docs = lists:map(fun (Result) -> mmap_to_doc(DocName, Result) end, Results),

  {ok, Docs, State}.

-spec find_all(sumo:schema_name(), state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, State) ->
  find_all(DocName, [], 0, 0, State).

-spec find_all(sumo:schema_name(),
               term(),
               non_neg_integer(),
               non_neg_integer(),
               state()) ->
  sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, SortFields, Limit, Offset, State) ->
  find_by(DocName, [], SortFields, Limit, Offset, State).

-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName, Conditions, #{conn := Connection} = State) ->
  Collection = atom_to_binary(DocName),
  TranslatedConditions = transform_conditions(DocName, Conditions),
  Selector = get_query(TranslatedConditions),

  {true, #{<<"n">> := Count}} = mc_worker_api:delete(Connection, Collection, Selector),

  {ok, Count, State}.

-spec delete_all(sumo:schema_name(), state()) ->
  sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(DocName, State) ->
  delete_by(DocName, [], State).

-spec count(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result(non_neg_integer(), state()).
count(DocName, State) ->
  count_by(DocName, [], State).


-spec count_by(DocName, Conditions, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  State      :: state(),
  Response   :: sumo_store:result(non_neg_integer(), state()).
count_by(DocName, Conditions, #{conn := Connection} = State) ->
  Collection = atom_to_binary(DocName),
  TranslatedConditions = transform_conditions(DocName, Conditions),
  Selector = get_query(TranslatedConditions),
  Count = mc_worker_api:count(Connection, Collection, Selector),

  {ok, Count, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Connection, _},
            #{conn := Connection, ref := Ref} = State) ->
  {stop, normal, State};
handle_info(_Msg, State) ->
  {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
sleep(Doc) ->
  sumo_utils:doc_transform(fun sleep_fun/4, Doc).

%% @private
sleep_fun(FieldType, _, FieldValue, _) when FieldType == date;
                                            FieldType == datetime ->
  case {FieldType, sumo_utils:is_datetime(FieldValue)} of
    {datetime, true} -> iso8601:format(FieldValue);
    {date, true}     -> iso8601:format({FieldValue, {0, 0, 0}});
    _                -> FieldValue
  end;
sleep_fun(custom, _, FieldValue, FieldAttrs) ->
  Type = sumo_utils:keyfind(type, FieldAttrs, custom),
  sleep_custom(FieldValue, Type);
sleep_fun(_, _, FieldValue, _) ->
  FieldValue.

%% @private
sleep_custom(FieldValue, FieldType) ->
  case lists:member(FieldType, [array, bin_data, object]) of
    true -> base64:encode(term_to_binary(FieldValue));
    _    -> FieldValue
  end.

%% @private
wakeup(Doc) ->
  sumo_utils:doc_transform(fun wakeup_fun/4, Doc).

%% @private
wakeup_fun(FieldType, _, FieldValue, _) when FieldType =:= datetime;
                                             FieldType =:= date ->
  case {FieldType, iso8601:is_datetime(FieldValue)} of
    {datetime, true} -> iso8601:parse(FieldValue);
    {date, true}     -> {Date, _} = iso8601:parse(FieldValue), Date;
    _                -> FieldValue
  end;
wakeup_fun(custom, _, FieldValue, FieldAttrs) ->
  Type = sumo_utils:keyfind(type, FieldAttrs, custom),
  wakeup_custom(FieldValue, Type);
wakeup_fun(_, _, FieldValue, _) ->
  FieldValue.

%% @private
wakeup_custom(FieldValue, FieldType) ->
  case lists:member(FieldType, [bin_data, array, object]) of
    true -> binary_to_term(base64:decode(FieldValue));
    _    -> FieldValue
  end.

%% @private
do_persist(update, Connection, Collection, Id, Fields) ->
  Command = #{<<"$set">> => Fields#{<<"_id">> => Id}},
  {true, _} = mc_worker_api:update(Connection, Collection, #{<<"_id">> => Id}, Command),
  ok;
do_persist(create, Connection, Collection, Id, Fields) ->
  {{true, _}, _} = mc_worker_api:insert(Connection, Collection, Fields#{<<"_id">> => Id}),
  ok.

%% @private
atom_to_binary(DocName) ->
  list_to_binary(atom_to_list(DocName)).

%% @private
sort_fields({Field, desc}) ->
  #{atom_to_binary(Field) => -1};
sort_fields({Field, asc}) ->
  #{atom_to_binary(Field) => 1}.

%% @private
mmap_to_doc(DocName, MongoMap) ->
  wakeup(sumo_internal:new_doc(DocName, mmap_to_map(MongoMap))).

%% @private
mmap_to_map(MongoMap) ->
  lists:foldl(fun({K, V}, Acc) ->
      maps:put(sumo_utils:to_atom(K), V, Acc)
  end, #{}, maps:to_list(MongoMap)).

%% @private
get_query(Conditions) ->
  lists:foldl(fun condition/2, #{}, Conditions).

%% @private
condition({'and', Conditions}, _Query) ->
  get_query(Conditions);
condition({'or', Conditions}, Query) ->
  Query#{<<"$or">> => get_or_query(Conditions)};
condition({'not', {FieldName, Operator, Value}}, Query) ->
  MOperator = mongo_operator(Operator, Value),
  Query#{atom_to_binary(FieldName) => #{<<"$not">> => MOperator}};
condition({FieldName, Operator, Value}, Query) ->
  MOperator = mongo_operator(Operator, Value),
  add_condition(atom_to_binary(FieldName), MOperator, Query);
condition({FieldName, 'null'}, Query) ->
  add_condition(atom_to_binary(FieldName), undefined, Query);
condition({FieldName, 'not_null'}, Query) ->
  add_condition(atom_to_binary(FieldName), #{<<"$ne">> => undefined}, Query);
condition({FieldName, FieldValue}, Query) ->
  add_condition(atom_to_binary(FieldName), FieldValue, Query).

%% @private
add_condition(Field, Value, Map) ->
  case maps:get(Field, Map, undefined) of
    undefined ->
      Map#{Field => Value};
    ActualValue when is_map(ActualValue) ->
      Map#{Field => resolve_conflicts(Value, ActualValue)};
    ActualValue when is_list(ActualValue) ->
      Map#{Field => [Value | ActualValue]};
    ActualValue ->
      Map#{Field => [Value, ActualValue]}
  end.

%% @private
resolve_conflicts(#{<<"$ne">> := Value} = NewOperator, ActualMap) ->
  case maps:get(<<"$ne">>, ActualMap, undefined) of
    undefined ->
      maps:merge(ActualMap, NewOperator);
    ActualNEValue ->
      case maps:get(<<"$nin">>, ActualMap, undefined) of
        undefined ->
          ActualMap#{<<"$nin">> => [Value, ActualNEValue]};
        ActualNINValue ->
          ActualMap#{<<"$nin">> => [Value | ActualNINValue]}
      end
  end;
resolve_conflicts(NewOperator, ActualMap) ->
  maps:merge(ActualMap, NewOperator).

%% @private
mongo_operator('<', Value) ->
  #{<<"$lt">> => Value};
mongo_operator('>', Value) ->
  #{<<"$gt">> => Value};
mongo_operator('==', Value) ->
  Value;
mongo_operator('=<', Value) ->
  #{<<"$lte">> => Value};
mongo_operator('>=', Value) ->
  #{<<"$gte">> => Value};
mongo_operator('/=', Value) ->
  #{<<"$ne">> => Value};
mongo_operator('like', Value) ->
  Regex = like_to_regex(Value),
  #{<<"$regex">> => Regex}.

%% @private
get_or_query(Conditions) ->
  % Each condition should be in a list in order to use get_query/1
  PreConditions = lists:map(fun(Condition) -> [Condition] end, Conditions),
  lists:map(fun get_query/1, PreConditions).

%% @private
transform_conditions(DocName, Conditions) ->
  sumo_utils:transform_conditions(
    fun validate_date/1, DocName, Conditions, [date, datetime]).

%% @private
validate_date({FieldType, _, FieldValue}) ->
  case {FieldType, sumo_utils:is_datetime(FieldValue)} of
    {datetime, true} ->
      iso8601:format(FieldValue);
    {date, true} ->
      DateTime = {FieldValue, {0, 0, 0}},
      iso8601:format(DateTime)
  end.

%% @private
like_to_regex(Like) ->
  Bin = sumo_utils:to_bin(Like),
  LikeList = sumo_utils:to_list(Like),
  Regex0 = binary:replace(Bin, <<"%">>, <<".*">>, [global]),
  Regex1 = case hd(LikeList) of
             $% -> Regex0;
             _ -> <<"^", Regex0/binary>>
           end,
  case lists:last(LikeList) of
    $% -> Regex1;
    _ -> <<Regex1/binary, "$">>
  end.
