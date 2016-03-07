# sumo_db_mongo

# About

This is the [MongoDB](https://www.mongodb.org/downloads) adapter for [sumo_db](https://github.com/inaka/sumo_db) that works
for **2.x** versions.


## MongoDB

### Install MongoDB

To install **MongoDB** please follow the instructions in this link:
[Install MongoDB](https://docs.mongodb.org/manual/installation/).

### Initial Configurations

Due to the fact that **MongoDB** comes with default configuration, we need to
change some parameters required by `sumo_db`.

If `sumo` is giving you an error like *exit with reason bad return value: <<"auth failed">> in context start_error*,
it means that your current `MongoDB` installation is using the
[SCRAM-SHA-1](https://docs.mongodb.org/manual/core/security-scram-sha-1/#authentication-scram-sha-1)
authentication mechanism, so we need to change it to use
[MongoDB-CR](https://docs.mongodb.org/manual/core/security-mongodb-cr/) following these steps:

```javascript
//NOTE: USE REMOVE COMMANDS IN TEST DB ONLY, IF IN PRODUCTION USE UPDATE.
mongo
use admin
db.system.users.remove({})    <== removing all users
db.system.version.remove({}) <== removing current version
db.system.version.insert({ "_id" : "authSchema", "currentVersion" : 3  })

//Now restart the mongod and create new user then it should work fine.

// Taken from http://stackoverflow.com/a/31476552/2969462
```

## Getting Started

To start use `sumo_db` with this MongoDB adapter `sumo_db_mongo` is pretty easy, you only has to
follow these steps:

 1. Add `sumo_db` and `sumo_db_mongo` as dependencies in your project.

Using **erlang.mk**:

```erlang
DEPS = sumo_db sumo_db_mongo

dep_sumo_db      = git https://github.com/inaka/sumo_db.git      0.5.0
dep_sumo_db_mongo = git https://github.com/inaka/sumo_db_mongo.git 0.0.1
```

Using **Rebar**:

```erlang
{deps, [
  {sumo_db, {git, "https://github.com/inaka/sumo_db.git", {tag, "0.5.0"}}},
  {sumo_db_mongo, {git, "https://github.com/inaka/sumo_db_mongo.git", {tag, "0.0.1"}}}
]}.
```

 2. You need at least one doc/entity, let's use [sumo_test_people_mongo](./test/sumo_test_people_mongo.erl)
    as example.
    > NOTE: if you use this entity, you'll need to include `mixer` to the dependencies list

 3. Provide the configuration file, e.g.: [test.config](./tests/test.config).

 4. Now you can run your app and start using `sumo` from there.

### Running sumo from Erlang console

Start the Erlang console, adding the path to your beams and config file

    $ erl -pa ebin deps/*/ebin -config tests/test.config

Within the console:

```erlang
> application:ensure_all_started(sumo_db_mongo).
15:18:39.914 [info] Application lager started on node nonode@nohost
15:18:39.964 [info] Application sasl started on node nonode@nohost
15:18:39.976 [info] Application emongo started on node nonode@nohost
15:18:39.995 [info] Application crypto started on node nonode@nohost
15:18:40.005 [info] Creating wpool ETS table
15:18:40.006 [info] Application worker_pool started on node nonode@nohost
15:18:40.010 [info] Application quickrand started on node nonode@nohost
15:18:40.011 [info] Application uuid started on node nonode@nohost
15:18:40.150 [info] Application sumo_db started on node nonode@nohost
15:18:40.155 [info] Application sumo_db_mongo started on node nonode@nohost
{ok,[syntax_tools,compiler,goldrush,lager,sasl,emongo,
     crypto,worker_pool,quickrand,uuid,sumo_db,sumo_db_mongo]}

% from here you can start using sumo

> sumo:find_all(sumo_test_people_mongo).
[]
```


## Running Tests

- Create a test database
```javascript
use sumo_test
```
- Create an user to access that DB.
```javascript
db.createUser({user: "root", pwd: "pass", roles: [{role: "userAdmin", db: "sumo_test"}]})
```
- Or use defaults and configure it on `test/test.config` file.


## TODO

- Make this adapter work with MongoDB **3.x**


## Contact Us

For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/sumo_db_mongo/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at
[inaka.github.io](http://inaka.github.io)
