from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pwsdl_nt_main',
          '-a', 'http://localhost:7718', 'src/wsdl_nt.ads',
          '-o', 'wsdl_nt.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'wsdl_nt_types', 'wsdl_nt.wsdl'])

build_and_run('wsdl_nt_main')
