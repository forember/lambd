# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

'''Defines a comment

(@@ c.(This is a comment!) x z) -> (x z)
'''

from lambd import *

def ext_parexpr(tree, extra_params, body):
  body.pop(1)
  tree[:] = parse_parexpr(body)
