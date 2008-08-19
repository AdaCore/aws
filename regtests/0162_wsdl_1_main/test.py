from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-I.', '-Pwsdl_1_main',
          '-a', 'http://localhost:7701', 'wsdl_1.ads', '-o', 'wsdl_1.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_1', 'wsdl_1.wsdl'])

build_diff('wsdl_1_main');
