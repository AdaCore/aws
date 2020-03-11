from test_support import *

# wsdl commited to not require ada2wsdl tool
# exec_cmd('ada2wsdl',
#     ['-q', '-f', '-I.', '-Pwsdl_nan_main',
#      '-a', 'http://localhost:7071', 'wsdl_nan.ads', '-o', 'wsdl_nan.wsdl'])

exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_nan', 'wsdl_nan.wsdl'])

build_and_run('wsdl_nan_main');
