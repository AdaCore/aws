from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-sp', '-doc', 'datafeed_service.wsdl'])
build_and_run('datafeed_service');
