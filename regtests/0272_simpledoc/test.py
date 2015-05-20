from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'simpledoc.wsdl'])
build_and_run('simpledoc')
