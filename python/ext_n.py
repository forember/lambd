# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''Parses the body normally

(@@n.expr) -> (expr)
'''

from lambd import *

def ext_parexpr(tree, extra_params, body):
  tree[:] = parse_parexpr(body)
