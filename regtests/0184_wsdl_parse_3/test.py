from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'wsdl_parse_3.wsdl'],
         ignore_error=True)
