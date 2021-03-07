# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''Enables church numeral literals

(@@ churchnums.+ 1 3) ->
((@3.(@1.+ 1 3)(@f.f))(@f.@x.f(f(f x))))
'''

from lambd import *

def ext_parexpr(tree, extra_params, body):
  if len(extra_params) != 1:
    raise LambdaSyntaxError('ext_churchnums')
  body = parse_parexpr(body)
  for var in FV(body):
    if var == '1':
      body = [APPL, [ABST, var, body], [ABST, 'f', [VAR, 'f']]]
    elif var.isdigit():
      valbody = [VAR, 'x']
      for x in range(int(var)):
        valbody = [APPL, [VAR, 'f'], valbody]
      body = [APPL, [ABST, var, body], [ABST, 'f', [ABST, 'x', valbody]]]
  tree[:] = body
