from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-Pwsdl_c', 'wsdl_c.ads',
          '-o', 'wsdl_c.wsdl'],
         output_file='ada2wsdl.res')

tail('ada2wsdl.res', None, 3)
