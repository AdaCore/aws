from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', 'ei.wsdl'],
         output_file='wsdl2aws.res')

build('ws.gpr')
tail('wsdl2aws.res', None, 0)
