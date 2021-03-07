# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''Flags an expression to be reduced before it is involved in substitution

(@@ b.expr)
(@@ b e.expr)
(@@ b b.expr)
'''

from lambd import *

def ext_parexpr(tree, extra_params, body):
  if len(extra_params) == 1:
    varname = '@b'
  elif len(extra_params) == 2:
    varname = '@b ' + extra_params[1]
  else:
    raise LambdaSyntaxError('ext_b')
  tree[0] = APPL
  tree[1] = [ABST, varname, [VAR, varname]]
  if body[:2] == ['@', 'b'] and len(body) <= 3:
    tree[:] = tree[1]
  else:
    tree[2] = parse_parexpr(body)
