from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'test_wsdl4.wsdl'])
build_and_run('test_wsdl4');
