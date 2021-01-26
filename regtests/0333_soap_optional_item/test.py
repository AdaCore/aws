from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'optional.wsdl'])
build_and_run('optional');
