from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', 'ConvertComputer.wsdl'])
build_and_run('test_conv');
