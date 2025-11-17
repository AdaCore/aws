from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'enumval.wsdl'])
build_and_run('enumval')
