from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'test_wsdl.wsdl'],
         ignore_error=True)
build_and_run('test_wsdl');
