from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_2_main',
          '-a', 'http://localhost:7702', 'wsdl_2.ads', '-o', 'wsdl_2.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_2', 'wsdl_2.wsdl'])

build_and_run('wsdl_2_main');
