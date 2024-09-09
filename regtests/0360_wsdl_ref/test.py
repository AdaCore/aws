from test_support import *

exec_cmd('wsdl2aws',
         ['-v', '-f', '-e', 'http://corp.com', './ws/w29/w30/9/w21_V1.9.0.wsdl'],
         output_file='wsdl2aws.res')

build('wsref.gpr')
tail('wsdl2aws.res', None, 0)
