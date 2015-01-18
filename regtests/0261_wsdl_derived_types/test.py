from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', '-v', 'alias.wsdl'],
         ignore_error=True)
build('alias')
