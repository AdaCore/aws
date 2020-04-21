from test_support import *
import re

build('mwsdl')

exec_cmd('ada2wsdl', ['-q', '-f', '-Pmwsdl.gpr', 'src/pck-serv1.ads',
                      '-o', 'serv1.wsdl'])

exec_cmd('ada2wsdl', ['-q', '-f', '-Pmwsdl.gpr', 'src/pck-serv2.ads',
                      '-o', 'serv2.wsdl'])

exec_cmd('wsdl2aws', ['-q', '-f', '-n', 'soapaws1', '-cb', '-types', 'pck',
                      '-spec', 'pck.serv1', '-p', 'srv1', 'serv1.wsdl'])

exec_cmd('wsdl2aws', ['-q', '-f', '-n', 'soapaws2', '-cb', '-types', 'pck',
                      '-spec', 'pck.serv2', '-p', 'srv2', 'serv2.wsdl'])

exec_cmd('awsascb', ['pck.serv1_service', 'pck.serv2_service'])

build('gen')
run('gobj/mwsdl')
