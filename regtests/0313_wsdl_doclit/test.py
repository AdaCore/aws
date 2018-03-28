from test_support import exec_cmd, build

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pdata', '-o', 'srv.wsdl',
          '-doc', '-lit', '-a', 'http://localhost:7710', 'data.ads'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-main', 'server',
          '-types', 'data', 'srv.wsdl'])

build('data')
