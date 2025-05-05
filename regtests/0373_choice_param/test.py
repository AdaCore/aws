from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', '-doc', 'choice_param.wsdl'],
         ignore_error=True)

build_and_run("choice_param_main")
