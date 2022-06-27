from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-sp', '-doc', 'test_wsdl4.wsdl'])
build_and_run('test_wsdl4');
