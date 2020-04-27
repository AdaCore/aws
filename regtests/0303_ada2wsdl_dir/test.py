from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_1b_main',
          '-a', 'http://localhost:7716', 'src/wsdl_1b.ads',
          '-o', 'wsdl_1b.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_1b', 'wsdl_1b.wsdl'])

build_and_run('wsdl_1b_main')
