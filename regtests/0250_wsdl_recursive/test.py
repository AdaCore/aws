from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', '-v', 'wsdl_recurse.wsdl'],
         ignore_error=True)
