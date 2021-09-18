from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'wsdl_h2hello.wsdl'])
build_and_run('wsdl_h2hello')
