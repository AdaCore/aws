from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_5_main',
          '-a', 'http://localhost:7705', 'wsdl_5.ads', '-o', 'wsdl_5.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_5', 'wsdl_5.wsdl'])

build_and_run('wsdl_5_main');
