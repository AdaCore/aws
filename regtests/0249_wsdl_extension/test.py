from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', '-v', 'wsdl_extension.wsdl'],
         ignore_error=True)
build('wsdlex')
