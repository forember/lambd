# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''Include a .lambd file

@@ include arithmetic.++ 0
'''

from lambd import *

included = []

def ext_parexpr(tree, extra_params, body):
  if len(extra_params) != 2:
    raise LambdaSyntaxError('ext_include')
  name = extra_params[1]
  included.append(name)
  with open(name + '.lambd') as f:
    s = f.read()
  toks = tokenize(s)
  tree[:] = parse_parexpr(parse_partree(toks) + [body])
