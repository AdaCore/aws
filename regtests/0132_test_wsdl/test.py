from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'test_wsdl.wsdl'])
build_diff('test_wsdl')
