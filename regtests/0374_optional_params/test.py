from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', '-doc', 'opt_param.wsdl'],
         ignore_error=True)

build_and_run("opt_param_main")
