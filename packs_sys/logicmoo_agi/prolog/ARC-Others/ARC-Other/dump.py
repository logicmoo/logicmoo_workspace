import json
import sys
import ast
from ast import parse
from ast import *
from pprint import pprint

#mode='exec', *,
tree = ast.parse((open(sys.argv[1]).read()), filename=sys.argv[1],  type_comments=True, feature_version=None)
print(ast.dump(tree))


#print 'Number of arguments:', len(sys.argv), 'arguments.'
#print 'Argument List:', str(sys.argv)

from ast2json import ast2json
from ast2json import *
#ast = ast2json(parse(open(sys.argv[0]).read()))
#print(str2json(open(sys.argv[1]).read()))

#print json.dump(ast, indent=2)

