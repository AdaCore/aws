from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-sp', '-doc', 'occurs.wsdl'])
build_and_run('occurs')
