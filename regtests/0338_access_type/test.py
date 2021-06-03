from test_support import *

exec_cmd('ada2wsdl', ['-f', 'at1.ads'], output_file="at1.out")
exec_cmd('ada2wsdl', ['-f', 'at2.ads'], output_file="at2.out")
