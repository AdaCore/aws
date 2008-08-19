from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'wsdl_parse_2.wsdl'],
         output_file='test.res', ignore_error=True)

diff()
