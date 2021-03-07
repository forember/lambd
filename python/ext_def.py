# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''"Defines" a variable

(@@ def x.(y z) x z) ->
((@x.x z)(y z))
'''

from lambd import *

def ext_parexpr(tree, extra_params, body):
  if len(extra_params) != 2:
    raise LambdaSyntaxError('ext_def')
  name = extra_params[1]
  value = body.pop(1)
  if value[0] != EXPR:
    value = [EXPR, value]
  tree[0] = APPL
  tree[1] = [ABST, name, parse_parexpr(body)]
  tree[2] = parse_parexpr(value)
