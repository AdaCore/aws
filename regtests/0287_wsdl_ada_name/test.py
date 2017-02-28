from test_support import *

exec_cmd('wsdl2aws', ['-q', '-a', '-f', '-doc', 'wsdl_ada_name.wsdl'])
build_and_run('wsdl_ada_name')
