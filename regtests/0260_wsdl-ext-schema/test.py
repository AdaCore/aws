from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', '-v', 'service.wsdl'], ignore_error=True)

build('extschema')
