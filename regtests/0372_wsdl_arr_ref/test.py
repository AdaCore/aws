from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'arr_ref.wsdl'])
build_and_run('arr_ref');
