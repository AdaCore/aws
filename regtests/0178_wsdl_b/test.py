from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-Pwsdl_b', 'wsdl_b.ads',
          '-o', 'wsdl_b.wsdl'],
         output_file='ada2wsdl.res')

tail('ada2wsdl.res', None, 3)
