kvstore
=====

An OTP application

Play
----

    $ rebar3 do compile, shell

This way you'll have a shell opened to try commands and show the logs. Then,
you can use the following ways to access to the data:

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
