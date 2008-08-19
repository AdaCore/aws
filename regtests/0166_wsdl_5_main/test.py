from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-I.', '-Pwsdl_5_main',
          '-a', 'http://localhost:7705', 'wsdl_5.ads', '-o', 'wsdl_5.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_5', 'wsdl_5.wsdl'])

build_diff('wsdl_5_main');
