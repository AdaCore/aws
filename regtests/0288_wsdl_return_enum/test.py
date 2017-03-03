import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-a', 'http://localhost:8787',
                      '-f', 'wsdl_ret_enum_data.ads', '-lit',
                      '-o', '../generated/wsdl_ret_enum.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'wsdl_ret_enum_data',
                      '-cb', '-main', 'wsdl_ret_data_service',
                      'wsdl_ret_enum.wsdl'])

os.chdir('..')
build_and_run('wsdl_ret_enum')
