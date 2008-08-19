from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-I.', '-Pwsdl_8_main',
          '-a', 'http://localhost:7708', 'wsdl_8.ads', '-o', 'wsdl_8.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_8', 'wsdl_8.wsdl'])

build_diff('wsdl_8_main');
