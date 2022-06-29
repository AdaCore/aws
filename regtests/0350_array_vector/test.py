from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'array_vector.wsdl'])
build_and_run('array_vector');
