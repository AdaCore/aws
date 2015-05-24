from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'simpledoc2.wsdl'])
build_and_run('simpledoc2')
