from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pdata_management', '-o', 'dm.wsdl',
          '-a', 'http://localhost:7709', 'data_management.ads'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-main', 'server',
          '-types', 'data_management', 'dm.wsdl'])

build('data_management')
