app
===

**app** is a simple application loader for Erlang. This tool can be used to easily load and run Erlang applications on remote embedded hardware such as the Lumenosys Robotics [BMOD][1].

Dependencies
------------

To build you will need a working installation of Erlang 17 (or
later). <br/>
Please refer to [Erlang/OTP](http://www.erlang.org) for information on building and installing Erlang/OTP.

This application is built using [rebar](https://github.com/rebar/rebar). Refer to [building rebar](https://github.com/rebar/rebar/wiki/Building-rebar) for information on building and using rebar.

Downloading
-----------

```sh
$ git clone git://github.com/lumenosys/app.git
```
Building
--------

Compile:

```sh
$ cd app
$ rebar compile app
...
==> app (compile)
```

Usage example
-------------

```sh
$ export ERL_LIBS=/path/to/myapp
$ erl -sname master@local -setcookie lumenosys -boot start_sasl -eval "app:start(obsidian@bmod, myapp)"
```

[1]: https://lumenosys.com/products