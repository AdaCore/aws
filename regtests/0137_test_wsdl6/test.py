from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'test_wsdl6.wsdl'])
build_and_run('test_wsdl6');
