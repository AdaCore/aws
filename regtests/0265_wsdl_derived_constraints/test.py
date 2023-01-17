from test_support import *

exec_cmd('wsdl2aws', ['-f', '-sp', '-doc', '-v', 'deriveconst.wsdl'],
         ignore_error=True)
build_and_run('deriveconst')
