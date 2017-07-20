-module(complete_coverage_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1,
  init_per_testcase/2
]).

%% Test Cases
-export([sumo_backend_mongo/1,
         sumo_db_mongo/1,
         sumo_store_mongo/1]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  init_per_testcase,
  end_per_suite
]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Common Test
%%%=============================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = test_utils:start_apps(),
  [{name, people} | Config].

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_, Config) ->
  {_, Name} = lists:keyfind(name, 1, Config),
  ok = sumo_basic_test_helper:init_store(Name),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  _ = sumo:delete_all(users),
  ok = test_utils:stop_apps(),
  Config.

-spec sumo_backend_mongo(config()) -> ok.
sumo_backend_mongo(_Config) ->
  ok = gen_server:cast(sumo_test_backend_mongo, coverage),
  sumo_test_backend_mongo ! coverage,
  State = #{mongo_options => []},
  ok = sumo_backend_mongo:terminate(normal, State),
  {ok, State} = sumo_backend_mongo:code_change(old_version, State, extra),
  ok.

-spec sumo_db_mongo(config()) -> ok.
sumo_db_mongo(_Config) ->
  {ok, []} = sumo_db_mongo:start(),
  ok.

-spec sumo_store_mongo(config()) -> ok.
sumo_store_mongo(_Config) ->
  ok = load_users(),
  2 = sumo:count_by(users, [{'not', {age, '>', 40}}]),
  3 = sumo:count_by(users, [{age, '/=', 33}, {age, '/=', 45}, {age, '/=', 4000}, {age, '/=', 400}]),
  ok.

%% @private
load_users() ->
  User = sumo_test_users_mongo:new(<<"Inako">>, <<"Garcia">>, 30),
  User2 = sumo_test_users_mongo:birthdate(User, <<"This is a bad date format">>),
  User3 = sumo_test_users_mongo:numbers(User2, 2.3),
  User4 = sumo_test_users_mongo:new(<<"Felipe">>, <<"Ripoll">>, 33),
  User5 = sumo_test_users_mongo:new(<<"Zinedine">>, <<"Zidane">>, 45),
  User6 = sumo_test_users_mongo:new(<<"Sauron">>, <<"the one">>, 5000),
  User7 = sumo_test_users_mongo:new(<<"John">>, <<"Mcdonalds">>, 55),
  _ = sumo:persist(users, User3),
  _ = sumo:persist(users, User4),
  _ = sumo:persist(users, User5),
  _ = sumo:persist(users, User6),
  _ = sumo:persist(users, User7),
  ok.
