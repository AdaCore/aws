import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-P', '../api.gpr', '-a', 'http://localhost:7878',
                      '-f', 'unbounded_string_subtype.ads', '-lit',
                      '-o', '../generated/unbounded_string_subtype.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-spec', 'unbounded_string_subtype',
                      '-cb', 'unbounded_string_subtype.wsdl'])

os.chdir('..')
build_and_run('soap_unbounded_string_subtype')
