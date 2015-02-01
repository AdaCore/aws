from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', '-doc', 'wsdl_choice.wsdl'],
         ignore_error=True)

build_and_run("wsdl_choice_main")
