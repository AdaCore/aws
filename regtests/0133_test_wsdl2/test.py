from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'test_wsdl2.wsdl'])
build_diff('test_wsdl2');
