from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'interoplab_main1.wsdl'])
build_and_run('interoplab_main1');
