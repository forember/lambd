lambd
=====

Parser-reducer for the lambda calculus, with a *dash* of syntax sugar.

Haskell Usage
-------------

***WIP***

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
