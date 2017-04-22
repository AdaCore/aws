import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-P', '../api.gpr', '-a', 'http://localhost:8787',
                      '-f', 'array_rec_int_data.ads', '-lit',
                      '-o', '../generated/array_rec_int.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'array_rec_int_types',
                      '-spec', 'array_rec_int_data',
                      '-cb', 'array_rec_int.wsdl'])

os.chdir('..')
build_and_run('array_rec_int')
