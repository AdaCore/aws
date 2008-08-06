from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'test_soap_timeout.wsdl'])
build_diff('test_soap_timeout');
