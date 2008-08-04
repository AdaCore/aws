from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'datafeed_service.wsdl'])
build_diff('datafeed_service');
