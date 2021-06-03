from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Ptest', '-o', 'test.wsdl', '-doc', '-lit',
          '-a', 'http://localhost:8212', 'fp.ads'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-debug', 'test.wsdl'])

build_and_run('test')
