from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', 'service.wsdl'], output_file='wsdl2aws.res')

build('test.gpr')
