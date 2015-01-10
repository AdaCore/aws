from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pd_management', '-o', 'dm.wsdl',
          '-a', 'http://localhost:7709', 'd_management.ads'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-main', 'server',
          '-types', 'd_management', 'dm.wsdl'])

build('d_management')
