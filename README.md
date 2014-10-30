Erlang Workshop
===============

## 0. Clone repository

```
$ git clone https://github.com/michalslaski/erlang-workshop
$ cd erlang-workshop
```

## 1. Create module

```
$ ./rebar create-lib libid=abacus
```

Edit src/abacus.erl and create 4 functions for arithmetic operations:
addition/2, subtraction/2, multiplication/2 and division/2. Save.

```
$ ./rebar compile
$ erl -pa ebin/
Erlang/OTP 17 [erts-6.1] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> abacus:addition(2,5).
7
```

## 2. Create tests

If you haven't done previous step, start with `git checkout 1-create-module`.

```
$ mkdir test
```

Edit test/abacus_tests.erl starting with

```
-include_lib("eunit/include/eunit.hrl").`
-module(abacus_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
```

Save.

```
$ ./rebar compile eunit
==> erlang-workshop (compile)
==> erlang-workshop (eunit)
Compiled src/abacus.erl
Compiled test/abacus_tests.erl
  All 4 tests passed.
```

## 3. Enable cover reports

If you haven't done previous step, start with `git checkout 2-create-tests`.

Edit rebar.config and add cover options

```
{cover_enabled,true}.
{cover_print_enabled,true}.
```

Save.

```
$ ./rebar eunit
==> erlang-workshop (eunit)
  All 4 tests passed.
Cover analysis: /Users/michalslaski/dev/erlang-workshop/.eunit/index.html

Code Coverage:
abacus : 100%

Total  : 100%

$ open .eunit/index.html
$ ./rebar doc
$ open doc/index.html
```

## 4. Create TCP server

If you haven't done previous step, start with `git checkout
3-create-rebar-config`.

Edit src/abacus_tcp.erl and use `gen_tcp:listen/2` to open a listening
TCP socket, `gen_tcp:accept/1` to accept client connections and
`receive {tcp, Socket, Command} -> ... end` to receive requests.

```
$ ./rebar compile
$ erl -pa ebin/
Erlang/OTP 17 [erts-6.1] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> abacus_tcp:start(1234).


$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
+12
+34
=
46
^]
telnet> quit
Connection closed.
```

## 5. Create gen_server

If you haven't done previous step, start with `git checkout 4-create-abacus-tcp`.

```
./rebar create template=simplesrv srvid=abacus_srv
```

Edit src/abacus_srv.erl and implement a server similar to
abacus_tcp.erl, but using the gen_server behaviour.

```
$ ./rebar compile
$ erl -pa ebin/
Erlang/OTP 17 [erts-6.1] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> abacus_srv:start_link().


$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
+12
*2
=
24
```


## 6. Create supervisor and application

If you haven't done previous step, start with `git checkout 5-create-abacus-gen-server`.

```
$ ./rebar create template=simpleapp appid=abacus
```

Edit src/abacus_sup.erl and add `?CHILD(abacus_srv, worker)` to the
supervision child spec. Edit src/abacus.app.src and add `{mod,
{abacus_app, []}}`, set `{vsn, "1.0"}` and set `{modules, []}`.

```
$ ./rebar compile
$ erl -pa ebin/
Erlang/OTP 17 [erts-6.1] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
1> application:start(abacus).
2> observer:start().
```

## 7. Create release

If you haven't done previous step, start with `git checkout 7-create-application`.

Edit relx.config and set `{release, {abacus, "1.0"}, [abacus]}.`

```
$ ./relx
$ _rel/abacus/bin/abacus
Erlang/OTP 17 [erts-6.1] [source-d2a4c20] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.1  (abort with ^G)
(abacus@127.0.0.1)1> application:which_applications().
[{abacus,"An Erlang abacus","1.0"},
 {stdlib,"ERTS  CXC 138 10","2.1"},
 {kernel,"ERTS  CXC 138 10","3.0.1"}]


$ telnet localhost 1234
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
+13
blabla
Connection closed by foreign host.
$ localhost 127.0.0.1 1234
Trying 127.0.0.1...
Connected to localhost.
```