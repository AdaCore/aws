from test_support import *

exec_cmd('wsdl2aws', ['-f', '-v', '-noskel', 'calculator.wsdl'],
         ignore_error=True)
build('calculator')
