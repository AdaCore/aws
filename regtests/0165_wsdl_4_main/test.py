from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_4_main',
          '-a', 'http://localhost:7704', 'wsdl_4.ads', '-o', 'wsdl_4.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_4', 'wsdl_4.wsdl'])

build_and_run('wsdl_4_main');
