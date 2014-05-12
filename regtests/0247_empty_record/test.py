from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', 'wsdl_empty_record.wsdl'],
         ignore_error=True)
build('wsdler')
