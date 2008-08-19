from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-I.', '-Pwsdl_a', 'wsdl_a.ads',
          '-o', 'wsdl_a.wsdl'],
         output_file='ada2wsdl.res')

tail('ada2wsdl.res', 'test.res', 3)
diff()
