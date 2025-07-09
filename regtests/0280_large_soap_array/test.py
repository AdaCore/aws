from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Plso_main',
          '-a', 'http://localhost:7712', 'lso.ads', '-o', 'lso.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-types', 'lso', 'lso.wsdl'])

build_and_run('lso_main')
