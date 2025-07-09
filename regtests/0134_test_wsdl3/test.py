from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-a', '-doc', 'test_wsdl3.wsdl'])
build_and_run('test_wsdl3');
