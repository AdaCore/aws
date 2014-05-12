from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', 'wsdl_element_type.wsdl'],
         ignore_error=True)
build('wsdlet')
