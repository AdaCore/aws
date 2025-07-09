from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_6_main',
          '-a', 'http://localhost:7706', 'wsdl_6.ads', '-o', 'wsdl_6.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_6', 'wsdl_6.wsdl'])

build_and_run('wsdl_6_main');
