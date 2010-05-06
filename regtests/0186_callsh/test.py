from test_support import *

exec_cmd('ada2wsdl',
         ['-q', '-d', '-f', '-I.', '-Pcallsh', 'callsh.ads',
          '-o', 'callsh.wsdl'])

with open('callsh.wsdl') as result:
    print result.read()
