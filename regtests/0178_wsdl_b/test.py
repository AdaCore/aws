from test_support import *

exec_cmd('ada2wsdl',
         ['-v', '-f', '-I.', '-Pwsdl_b', 'wsdl_b.ads',
          '-o', 'wsdl_b.wsdl'],
         output_file='ada2wsdl.res')

tail('ada2wsdl.res', 'test.res', 3)
diff()
