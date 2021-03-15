lambd
=====

Parser-reducer for the lambda calculus, with a *dash* of syntax sugar.

There are Haskell and Python implementations. The Python implementation is
around 6 or 7 years older and *considerably* slower, but is included here for
historical purposes.

Haskell Usage
-------------

**lambd** *[file]*

If *file* is provided, reduce the lambda expression in the named file.
Otherwise, reduce the expression read from stdin.

```sh
$ cd haskell
$ make
$ ./lambd tests/math.lambd
@@churchnums.[ 5040(@f.f 3 30) ]
```

Python Usage
------------

**lambd.py** *[module]*

If *module* is provided, reduce the `main` in the named module.  
Otherwise, reduce the expression read from stdin.

```sh
$ cd python
$ ./lambd.py tests/math
@@churchnums.[ 120(@f.f 3 3) ]
    calculated in 1012 cycles
      0.179032885 CPU s
      0.17912530899047852 s
```
