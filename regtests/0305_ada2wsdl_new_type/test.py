from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_ntr_main',
          '-a', 'http://localhost:7714', 'src/wsdl_ntr.ads',
          '-o', 'wsdl_ntr.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_ntr_types', 'wsdl_ntr.wsdl'])

build_and_run('wsdl_ntr_main')
