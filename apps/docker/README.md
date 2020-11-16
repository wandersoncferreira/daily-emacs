# Docker


## Access inside a docker container

You can use TRAMP mode to access a file inside a docker container:

``` shell
docker run --rm -p 6379:6379 --name redis_container redis
```

And then look at files inside of it using

``` shell
C-x C-f /docker:redis_container:/
```

## Access inside a docker container on a remote host

We can also chain things together. Let's say that we have a `docker`
container named `ssb-pub` running on a remote host
`ssb.willschenk.com`, we can connect to it using:

``` shell
C-x C-f /ssh:root@ssb.willschenk.com|docker:ssb-pub:/
```
