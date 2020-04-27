from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_9_main',
          '-a', 'http://localhost:7709', 'wsdl_9.ads', '-o', 'wsdl_9.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_9', 'wsdl_9.wsdl'])

build_and_run('wsdl_9_main');
