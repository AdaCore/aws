from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_d_main',
          '-a', 'http://localhost:7719', 'wsdl_d.ads', '-o', 'wsdl_d.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_d', 'wsdl_d.wsdl'])

build_and_run('wsdl_d_main');
