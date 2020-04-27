from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_types_pck_main',
          '-s', 'wsdl_types_pck',
          '-a', 'http://localhost:7717', 'types.ads',
          '-o', 'wsdl_types_pck.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'types', 'wsdl_types_pck.wsdl'])

build_and_run('wsdl_types_pck_main')
