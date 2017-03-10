from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-lit', '-f', '-o', 'api-child.wsdl', 'api-child.ads'])

exec_cmd('wsdl2aws', ['-q', '-f', 'api-child.wsdl'])
build_and_run('multiple_schema')
