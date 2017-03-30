import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-P', '../api.gpr', '-a', 'http://localhost:8787',
                      '-f', 'wsdl_enum_array_data.ads', '-lit',
                      '-o', '../generated/wsdl_enum_array.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'wsdl_enum_array_types',
                      '-spec', 'wsdl_enum_array_data',
                      '-cb', 'wsdl_enum_array.wsdl'])

os.chdir('..')
build_and_run('wsdl_enum_array')
