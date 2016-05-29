kvstore
=====

[![Build Status](https://img.shields.io/travis/manuel-rubio/kvstore/master.svg)](https://travis-ci.org/manuel-rubio/kvstore)
[![Codecov](https://img.shields.io/codecov/c/github/manuel-rubio/kvstore.svg)](https://codecov.io/gh/manuel-rubio/kvstore)
[![License: LGPL 2.1](https://img.shields.io/github/license/manuel-rubio/kvstore.svg)](https://raw.githubusercontent.com/manuel-rubio/kvstore/LICENSE)

An OTP application

Play
----

    $ rebar3 do compile, shell

This way you'll have a shell opened to try commands and show the logs. Then,
you can use the following ways to access to the data:

Available Commands
------------------

- **GET** `<key>`: retrieve the content for the specified *key*.
- **PUT** `<key>` `<value>`: set the *value* for the specified *key*.
- **POST** `<key>` `<value>`: same as **PUT** (in this system there are no difference between POST and PUT).
- **DELETE** `<key>`: remove the specified *key*.
- **QUIT**: close the connection (only available in TCP).

In case of HTTP, the *method* is the specified command and the *key* is the URI. The first `/` will be ignored. For example:

```
GET /mykey HTTP/1.1
Host: 127.0.0.1:5555

```

Is the same as the following command in TCP/UDP:

```
GET mykey
```

And the value for the PUT/POST commands is the body of the request. For example:

```
PUT /mykey HTTP/1.1
Host: 127.0.0.1:5555
Content-type: text/plain
Content-length: 5

hello
```

Is the same as the following command in TCP/UDP:

```
PUT mykey hello
```

TCP
---

In another shell you can execute the following command:

    $ telnet 127.0.0.1 5555
    PUT mykey myvalue
    OK
    GET mykey
    myvalue
    QUIT

UDP
---

You can use _netcat_ (`nc` command) for TCP or UDP, but I think there are no a
lot of options to work with UDP so:

    $ nc -u 127.0.0.1 5555
    PUT mykey mynewvalue
    OK
    GET mykey
    mynewvalue
    DELETE mykey
    OK
    GET mykey
    null
    ^C

Note that the only way to exit from `nc` is pressing Ctrl+C.

HTTP
----

This time you can use even the browser with Postman or another REST utility,
I'm going to try with cURL:

```
$ curl -i -X PUT --data-raw 'myoldvalue' http://127.0.0.1:5050/mykey
HTTP/1.1 200 OK
content-length: 0

$ curl -i http://127.0.0.1:5050/mykey
HTTP/1.1 200 OK
content-type: text/plain
content-length: 10

myoldvalue
```

In this case we use the methods GET, PUT/POST and DELETE to handle the
information. The key will be the URI and the value the body of the request.


TODO
----

- Change ad-hoc TCP transport to use `ranch` instead. This way is better because we can decrease the latency in very frequent requests.

- Change ad-hoc HTTP transport to use `cowboy` instead (same reason as previous one).

- Create workers in different processes in a pool to accept and process UDP requests. So the main process with the open port is there only to accept requests from the port to the workers and from the workers to outside.

- Configure ETS table as mnesia table and provide specific functions to spread in several nodes, not only one.

- Load tests to see the performance of the system.
