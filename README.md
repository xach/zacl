# ZACL

ZACL is for running
[AllegroServe](https://github.com/franzinc/allegroserve/) on
non-AllegroCL Lisps. The goal is to run AllegroServe sources with as
few modifications as possible. That means *occasionally* resorting to
grody one-off hacks instead of elegant beautiful gracefulness.

Where possible, ACL functionality is mimicked with portability
libraries like uiop, bordeaux-threads, cl-ppcre, USOCKET, etc. Where
necessary, implementation-specific hacks are used to fill in the gaps.




