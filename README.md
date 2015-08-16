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

Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Lumenosys Robotics 2014-2015. All Rights Reserved.
>
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
>
>     http://www.apache.org/licenses/LICENSE-2.0
>
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
>
> %CopyrightEnd%


[1]: https://lumenosys.com/products