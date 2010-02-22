from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', 'hello2.wsdl'],
         output_file='test.res', ignore_error=True)

diff()
