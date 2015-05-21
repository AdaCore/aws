from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'doclit.wsdl'])
build_and_run('doclit')
