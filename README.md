# sumo_db_mongo

# About

This is the [MongoDB](https://www.mongodb.org/downloads) adapter for [sumo_db](https://github.com/inaka/sumo_db) that works
for **3.x** versions.

## Contact Us

For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/sumo_db_mongo/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at
[inaka.github.io](http://inaka.github.io)

## MongoDB

### Install MongoDB

To install **MongoDB** please follow the instructions in this link:
[Install MongoDB](https://docs.mongodb.org/manual/installation/).

### Initial Configurations

Due to the fact that **MongoDB** comes with default configuration, we need to
change some parameters required by `sumo_db`.

## Getting Started

To start using `sumo_db` with this MongoDB adapter `sumo_db_mongo` is pretty easy, you only has to
follow these steps:

 1. Add `sumo_db_mongo` as dependency in your `rebar.config` file.

```erlang
{deps, [
  {sumo_db_mongo, {git, "git://github.com/inaka/sumo_db_mongo.git", {branch, "master"}}}
]}.
```

 2. You need at least one doc/entity, let's use [sumo_test_people_mongo](./test/sumo_test_people_mongo.erl)
    as example.
    > NOTE: if you use this entity, you'll need to include `mixer` to the dependencies list

 3. Provide the configuration file, e.g.: [test.config](./tests/test.config).

 4. Now you can run your app and start using `sumo` from there.

### Configuration
In order to connect with a _MongoDB_ database we have to set some configuration parameters in the config file.

```erlang
...
{sumo_db,
[
 {wpool_opts, [{overrun_warning, 100}]},
 {log_queries, true},
 {query_timeout, 30000},
 {storage_backends,
  [{sumo_test_backend_mongo,
    sumo_backend_mongo,
    [
     {login,    "root"},
     {password, "password"},
     {host,     "127.0.0.1"},
     {port,     27017},
     {database, "sumo_test"}
     ]
   }]
 }
...
```

- `login` and `password`. The credentials in order to connect to a database. By default _MongoDB_ has no authentication enabled.
- `host` is the host. By default is "127.0.0.1"
- `port` is the port. By default is 27017
- `database` the name of the database. *Mandatory*

### Running sumo from Erlang console

Start the Erlang console, adding the path to your beams and config file

    $ erl -pa _build/default/lib/*/ebin -pa _build/testib/sumo_db_mongo/test -config test/test.config -s sumo_db_mongo

Within the console:

```erlang
Eshell V8.3  (abort with ^G)
1> sumo:find_all(users).
[]
```

## Running Tests

First we need to add the connection parameters to `test/test.config` file as we described in *Configuration* section. After that just run:

      rebar3 ct
