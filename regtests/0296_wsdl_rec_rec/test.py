import os
from test_support import *

os.mkdir('generated')
os.mkdir('obj')
os.mkdir('bin')

exec_cmd('ada2wsdl', ['-q', '-Papi.gpr', '-a', 'http://localhost:8787',
                      '-f', 'src/enumeration_test.ads', '-lit',
                      '-o', 'generated/enumeration_demo.wsdl'])

os.chdir('generated')
exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'enumeration_types',
                      '-spec', 'enumeration_test',
                      '-cb', '-main', 'enumeration_server',
                      'enumeration_demo.wsdl'])

os.chdir('..')
build('wsdl_rec_rec')
