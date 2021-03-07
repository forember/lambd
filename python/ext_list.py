# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''Helper for creating lists

(@@ list e.x y z) ->
(@e.pair x (pair y (pair z e)))
'''

from lambd import *

def ext_parexpr(tree, extra_params, body):
  if len(extra_params) != 2:
    raise LambdaSyntaxError('ext_def')
  name = extra_params[1]
  new = [EXPR, '@', name, '.', name]
  ptr = new
  for value in body[1:]:
    ptr[-1] = [EXPR, 'pair', value, name]
    ptr = ptr[-1]
  tree[:] = parse_parexpr(new)
