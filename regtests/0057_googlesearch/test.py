from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'googlesearch.wsdl'])
build_diff('googlesearch');
