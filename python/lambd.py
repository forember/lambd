#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# vim: set sw=2 ts=2 sts=2 et :

import re
from collections import *
from traceback import print_exc
import random, copy, itertools
import sys

# = Syntax ===============================

class LambdaSyntaxError (Exception):
  pass

# Expression Types
EXPR = 0
VAR  = 1
APPL = 2
ABST = 3

def parse(s):
  '''Parse a source string into an abstract syntax tree.'''
  toks = tokenize(s)
  tree = parse_partree(toks)
  return parse_parexpr(tree)

token_re = re.compile(r'[@.()]|[^\s@.()]+')
def tokenize(s):
  '''Split a source string into tokens.'''
  return token_re.findall(s)

def parse_partree(toks):
  '''Identify parentheticals and tag them with EXPR.'''
  tree = [EXPR]
  branchdeque = deque()
  branch = tree
  for tok in toks:
    if tok == '(':
      branch.append([EXPR])
      branchdeque.append(branch)
      branch = branch[-1]
    elif tok == ')':
      branch = branchdeque.pop()
    else:
      branch.append(tok)
  return tree

var_re = re.compile(r'[^\s@.()]+$')
def isvar(tok):
  '''Is the token the name of a variable?'''
  try:
    return bool(var_re.match(tok))
  except:
    return False

def parse_parexpr(tree):
  '''Convert a token string with parentheticals identified into an AST.'''
  #print(tree); print()
  if len(tree) == 2:
    # Variable
    if isvar(tree[1]):
      tree[0] = VAR
    else:
      #print('parse expression')
      tree[:] = parse_parexpr(tree[1])
  elif tree[1] == '@':
    # Extension/Abstraction Prep
    tree[0] = ABST
    doti = tree.index('.')
    extra_params = tree[3:doti]
    body = [EXPR] + tree[doti+1:]
    tree[1] = tree[2]
    tree[2:] = [None]
    if tree[1] == '@':
      # Extensions
      import importlib
      ext = importlib.import_module('ext_' + extra_params[0])
      #print('extension parse')
      ext.ext_parexpr(tree, extra_params, body)
    else:
      # Abstraction
      branch = tree
      for param in extra_params:
        if isvar(param):
          branch[2] = [ABST, param, None]
          branch = branch[2]
        else:
          raise LambdaSyntaxError
      #print('parse after dot')
      branch[2] = parse_parexpr(body)
  else:
    # Application
    tree[0] = APPL
    if '@' in tree:
      lambdai = tree.index('@')
      tree[lambdai:] = [[ABST] + tree[lambdai:]]
    if len(tree) == 3:
      for i in (1, 2):
        if isvar(tree[i]):
          tree[i] = [VAR, tree[i]]
        else:
          #print('parse tree[{}]'.format(i))
          tree[i] = parse_parexpr(tree[i])
    else:
      if isvar(tree[-1]):
        tree[-1] = [VAR, tree[-1]]
      else:
        #print('parse last expression')
        tree[-1] = parse_parexpr(tree[-1])
      #print('parse as application')
      tree[1:-1] = [parse_parexpr([APPL] + tree[1:-1])]
  #print('return')
  return tree

def unparse(ast):
  '''Convert an AST back to a source string.'''
  s = ''
  type = ast[0]
  if type == VAR:
    s += ast[1]
  elif type == ABST:
    s += '@' + ast[1] + '.' + unparse(ast[2])
  elif type == APPL:
    if ast[1][0] == ABST:
      s += '(' + unparse(ast[1]) + ')'
    else:
      s += unparse(ast[1])
      if ast[2][0] == VAR:
        s += ' '
    if ast[2][0] != VAR:
      s += '(' + unparse(ast[2]) + ')'
    else:
      s += unparse(ast[2])
  return s

num_re = re.compile( \
  r'(?P<nvb>^|(?<=[\s@.()]))?' \
  r'((?P<op>\()|(?P<start>^))' \
  r'@(?P<f>[^.]+).' \
  r'(' \
    r'(?P<one>(?P=f))' \
  r'|' \
    r'@(?P<x>[^.]+).' \
    r'(?P<fs>' \
      r'(?P<fp>(?P=f)\()*' \
      r'(?(fp)(?P=f) )' \
    r')' \
    r'(?P=x)' \
  r')' \
  r'(?(op)\))' \
  r'(?(fs)(?P<cps>\)*))' \
  r'(?(start)$)' \
  r'(?P<nva>$|(?=[\s@.()]))?' \
  )
def restore_nums(s):
  '''Identify church numeral structures in a source string and replace them.'''
  i = 0
  while True:
    m = num_re.search(s, i)
    if not m:
      break
    #print('{!r} {}'.format(repr(m.group()), m.groupdict()))
    if m.group('one') != None:
      num = 1
      n_cps = 1
    else:
      fp_w = len(m.group('f')) + 1
      num = len(m.group('fs')) // fp_w
      n_cps = len(m.group('cps'))
      if num:
        n_cps += 1
    end_i = m.end()
    if n_cps < num:
      i = end_i
      continue
    elif n_cps == num:
      v_after = m.group('nva') == None
    elif n_cps > num:
      end_i -= n_cps - num
      v_after = False
    start_i = m.start()
    v_before = m.group('nvb') == None
    r_str = (' ' if v_before else '') + str(num) + (' ' if v_after else '')
    s = s[:start_i] + r_str + s[end_i:]
    i += len(r_str)
  return s

def repr_ast(ast, indent=''):
  '''Represent an abstract syntax tree with indentation.'''
  type = ast[0]
  if type == VAR:
    return '{}[VAR, {!r}]'.format(indent, ast[1])
  elif type == ABST:
    return '{indent}[ABST, {!r},\n{}\n{indent}]'.format(ast[1], repr_ast(ast[2], indent + ' '), indent=indent)
  elif type == APPL:
    return '{indent}[APPL,\n{},\n{}\n{indent}]'.format(repr_ast(ast[1], indent + ' '), repr_ast(ast[2], indent + ' '), indent=indent)

# = Semantics ============================

class ReductionError (Exception):
  pass

class StructureError (ReductionError):
  pass

def FV(expr):
  '''Identify free variables in an abstract syntax tree.'''
  type = expr[0]
  if type == VAR:
    return {expr[1]}
  elif type == ABST:
    return FV(expr[2]) - {expr[1]}
  elif type == APPL:
    return FV(expr[1]) | FV(expr[2])

def alpha_convert(abst, new_name):
  if abst[0] != ABST or new_name in FV(abst):
    raise StructureError
  abst[2] = substitute(abst[2], abst[1], [VAR, new_name])
  abst[1] = new_name
  return abst

def substitute(expr, name, rexpr, rfv=None):
  if rfv == None:
    if expr[0] == APPL and expr[1][0] == ABST and expr[1][1] in ('@b e', '@b b'):
      reduce_full(expr)
    if rexpr[0] == APPL and rexpr[1][0] == ABST and rexpr[1][1] in ('@b', '@b b'):
      reduce_full(rexpr)
    rfv = FV(rexpr)
  type = expr[0]
  if type == VAR:
    if name == expr[1]:
      expr[:] = copy.deepcopy(rexpr)
  elif type == APPL:
    for i in (1, 2):
      expr[i] = substitute(expr[i], name, rexpr, rfv)
  elif type == ABST:
    param = expr[1]
    if param != name:
      if param in rfv:
        alpha_convert(expr, param + '_' + format(random.randint(0, 0xffff), '04X'))
      expr[2] = substitute(expr[2], name, rexpr, rfv)
  return expr

def beta_reduce(appl):
  if appl[0] != APPL or appl[1][0] != ABST:
    raise StructureError
  appl[:] = substitute(appl[1][2], appl[1][1], appl[2])
  return appl

def eta_convert(abst):
  if abst[0] != ABST or abst[2][0] != APPL or abst[1] != abst[2][2][1] or abst[1] in FV(abst[2][1]):
    raise StructureError
  abst[:] = abst[2][1]
  return abst

def reduce_expr(expr):
  expr_stack = deque([expr])
  while True:
    try:
      expr = expr_stack.pop()
    except IndexError:
      return False
    type = expr[0]
    if callable(type):
      try:
        type(expr[1])
        return True
      except ReductionError:
        pass
    elif type == ABST:
      expr_stack.extend([expr[2], (eta_convert, expr)])
    elif type == APPL:
      expr_stack.extend([expr[2], expr[1]])
      expr_stack.append((beta_reduce, expr))

cycle_ctr = 0

def reduce_full(expr, ct_cycles=False):
  global cycle_ctr
  if ct_cycles:
    cycle_ctr = 0
  ct = 0
  while reduce_expr(expr):
    ct += 1
    #print('@@churchnums.' + restore_nums(unparse(expr))); print()
  cycle_ctr += ct
  if ct_cycles:
    ct = cycle_ctr
    cycle_ctr = 0
    return ct

# = Testing ==============================

def main():
  #sys.setrecursionlimit(10000)
  if len(sys.argv) >= 2:
    src = '@@include {}.main'.format(sys.argv[1])
  else:
    src = sys.stdin.read()
  expr = parse(src)
  import time
  t = time.time()
  cpu_t = time.process_time()
  #print(unparse(expr)); print()
  cycles = reduce_full(expr, True)
  cpu_t = time.process_time() - cpu_t
  t = time.time() - t
  print('@@churchnums.' + restore_nums(unparse(expr)))
  print('''    calculated in {} cycles
      {} CPU s
      {} s
'''.format(cycles, cpu_t, t))

if __name__ == '__main__':
  try:
    main()
    #import cProfile; cProfile.run('main()')
  except:
    print_exc()
    raise
