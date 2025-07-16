from test_support import *

exec_cmd('ada2wsdl',
         ['-f', '-Psoapcheck', 'pck.ads', '-o', 'pck.wsdl'],
         output_file="ada2wsdl.res")
tail("ada2wsdl.res", None, 3)
