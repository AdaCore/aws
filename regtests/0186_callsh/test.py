from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-d', '-f', '-I.', '-Pcallsh', 'callsh.ads',
          '-o', 'callsh.wsdl'])

diff('test.out', 'callsh.wsdl')
