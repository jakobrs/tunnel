# tunnel - `SSH -L`-like TCP and Unix tunnels

Usage:

```
tunnel <format> <arguments>

The following formats are supported:

-   p:h:p - local port, remote host, remote port
- h:p:h:p - local host, local port, remote host, remote port
-   p:s   - local port, remote unix
- h:p:s   - local host, local port, remote unix
-   s:h:p - local unix, remote host, remote port
-   s:s   - local unix, remote unix

Examples:

$ tunnel p:h:p 3000 localhost 80
$ tunnel p:s   3000 ./socket
```
