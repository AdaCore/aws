from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', 'hello2.wsdl'],
         ignore_error=True)
