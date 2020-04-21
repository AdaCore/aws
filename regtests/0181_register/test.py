from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-Pregister', 'register.ads',
          '-o', 'register.wsdl'],
         output_file='ada2wsdl.res')

tail('ada2wsdl.res', None, 3)
