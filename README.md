# ZACL

ZACL is for running
[AllegroServe](https://github.com/franzinc/allegroserve/) on
non-AllegroCL Lisps. The goal is to run AllegroServe sources with as
few modifications as possible. That means *occasionally* resorting to
grody one-off hacks instead of elegant beautiful gracefulness.

Where possible, ACL functionality is mimicked with portability
libraries like [UIOP](http://quickdocs.org/UIOP/),
[bordeaux-threads](http://quickdocs.org/bordeaux-threads/),
[cl-ppcre](http://weitz.de/cl-ppcre/),
[USOCKET](https://common-lisp.net/project/usocket/), etc. Where
necessary, implementation-specific hacks are used to fill in the gaps.

For example, Allegro CL's non-standard `#+(version>= ...)` expression
is sprinkled throughout aserve code. To work around the problem
without changing the code, zacl installs a readtable that overrides
the standard `#+` and `#-` readers to check for these expressions and
work around them.

Occasionally, a quirk in aserve's source is very difficult to work
around. In those cases, I will send patches to Franz, like for
[double-colon syntax in packages.cl](https://github.com/franzinc/aserve/pull/6),
[obsolete declarations not recognized in other Lisps](https://github.com/franzinc/aserve/pull/7),
[non-standard assumptions in array initialization](https://github.com/franzinc/aserve/pull/8),
etc.

Until those patches are merged or rejected, the
[zacl-compatible aserve fork](https://github.com/xach/aserve/tree/zacl-compatible)
on github has a lightly modified version of aserve that can be used
with zacl to use modern aserve on non-Allegro Lisps. You can check it
out like so:

    git clone -b zacl-compatible https://github.com/xach/aserve.git

## Usage

ZACL defines the packages and functionality that the aserve code
relies upon. But aserve's system definition does not include a
dependency on ZACL. There are a few ways to make sure ZACL is loaded
before aserve.

First, you can manually load ZACL before loading aserve:

	(ql:quickload "zacl")
	(ql:quickload "aserve")

Second, you can load the "zacl-aserve" shim system:

	(ql:quickload "zacl-aserve")

## Limitations

**Only works on Clozure CL**. This is because of time
constraints. Clozure CL's multiprocess API is the same as Allegro's,
so it was trivial to support immediately. There is a tiny bit of
simple-streams support that also relies directly on Clozure stream
functionality, but creating a portability layer that matches the
Clozure/Allegro multiprocess API would help ZACL work on many more
Lisps.

**Does not support CGI**. Another victim of time constraints. I do not
need CGI. CGI functionality requires non-trivial support for managing
subprocesses, and I am not sure if e.g. UIOP's subprocess management
API is sufficient.

**Lightly tested**. My application seems to work. Yours might not. If
you have trouble with ZACL and aserve, feel free to
[create an issue](https://gitlab.common-lisp.net/zbeane/zacl/issues)
and I'll take a look.

**Possibly slow**. Using generic functions and portability layers will
be slower than running aserve natively on Allegro CL. There aren't
many ways around it. If you have a specific performance issue that
you'd like ZACL to address, [get in touch](mailto:xach@xach.com).

