from test_support import *

exec_cmd('wsdl2aws',
         ['-v', '-f', '-e', 'http://corp.com', 'rst.wsdl'],
         output_file='wsdl2aws.res')

build('wsrst.gpr')
tail('wsdl2aws.res', None, 0)
