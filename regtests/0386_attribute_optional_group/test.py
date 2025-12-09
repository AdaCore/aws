from test_support import *

exec_cmd('wsdl2aws', ['-q', '-debug', '-f', 'attsimcont.wsdl'])
build_and_run('attsimcont')
