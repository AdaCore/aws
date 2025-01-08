from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', '-doc', 'nested_types.wsdl'],
         ignore_error=True)

build_and_run("nested_types_main")
