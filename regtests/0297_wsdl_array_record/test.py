import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-P', '../api.gpr', '-a', 'http://localhost:8787',
                      '-f', 'wsdl_array_record_data.ads', '-lit',
                      '-o', '../generated/wsdl_array_record.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'wsdl_array_record_types',
                      '-spec', 'wsdl_array_record_data',
                      '-cb', 'wsdl_array_record.wsdl'])

os.chdir('..')
build_and_run('wsdl_array_record')
