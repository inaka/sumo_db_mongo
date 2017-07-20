-module(sumo_test_users_mongo).

-behavior(sumo_doc).

%% @todo remove this once mixer migrates specs better
-dialyzer([no_behaviours]).

-export([new/3, birthdate/2, numbers/2]).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

-type user() :: #{_ => _}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% sumo_doc callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new(binary(), binary(), integer()) -> user().
new(Name, LastName, Age) ->
  #{name => Name,
    last_name => LastName,
    age => Age}.

-spec birthdate(user(), term()) -> user().
birthdate(User, Birthdate) ->
  User#{birthdate => Birthdate}.

-spec numbers(user(), term()) -> user().
numbers(User, Numbers) ->
  User#{numbers => Numbers}.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  Fields =
    [sumo:new_field(id,        string, [id]),
     sumo:new_field(name,      string),
     sumo:new_field(last_name, string),
     sumo:new_field(birthdate, date),
     sumo:new_field(numbers,   custom, [{type, numbers}]),
     sumo:new_field(age,       integer)
    ],
  sumo:new_schema(users, Fields).

-spec sumo_sleep(user()) -> map().
sumo_sleep(User) ->
  User.

-spec sumo_wakeup(map()) -> user().
sumo_wakeup(User) ->
  User.
