from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-debug', 'wsdl_call.wsdl'])
build_and_run('wsdl_call')
