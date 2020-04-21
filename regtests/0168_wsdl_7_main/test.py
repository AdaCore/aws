from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_7_main',
          '-a', 'http://localhost:7707', 'wsdl_7.ads', '-o', 'wsdl_7.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_7', 'wsdl_7.wsdl'])

build_and_run('wsdl_7_main');
