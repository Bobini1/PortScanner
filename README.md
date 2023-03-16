# PortScanner

A rewrite of the first assignment from Concurrent and Distributed Programming at Reykjavik University from Golang to Haskell.

Usage:

sample-exe <host> <host> -p <port>,<port>-<port>

A host can be an IP address, a hostname like "google.com" or a CIDR network specification (e.g. 127.0.0.1/24).

Ports are comma-separated. Ranges (e.g. 70-90) are supported too.
