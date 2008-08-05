from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-f', '-I.', '-Psoapcheck', 'pck.ads', '-o', 'pck.wsdl'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-spec', 'pck', 'pck.wsdl'])

build_diff('soapcheck');
