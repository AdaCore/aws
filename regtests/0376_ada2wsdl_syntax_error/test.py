from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Psoapcheck', 'pck.ads', '-o', 'pck.wsdl'],
         output_file="ada2wsdl.res")
