from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Psoapcheck', 'pck.ads', '-o', 'pck.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-spec', 'pck', 'pck.wsdl'])

build_and_run('soapcheck');
