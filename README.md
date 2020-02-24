# tunnel - `SSH -L`-like TCP and Unix tunnels

Usage:

```
tunnel <format> <arguments>

The following formats are supported:

-   p:p   - local port, remote port
-   p:h:p - local port, remote host, remote port
- h:p:p   - local host, local port, remote port
- h:p:h:p - local host, local port, remote host, remote port
-   p:u   - local port, remote unix
- h:p:u   - local host, local port, remote unix
-   u:p   - local unix, remote port
-   u:h:p - local unix, remote host, remote port
-   u:u   - local unix, remote unix

Examples:

$ tunnel p:h:p 3000 localhost 80
$ tunnel p:s   3000 ./socket
```
