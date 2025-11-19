from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'recfields.wsdl'])
build_and_run('recfields')
