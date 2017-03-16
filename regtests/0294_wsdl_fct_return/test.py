import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-a', 'http://localhost:8787',
                      '-f', 'wsdl_fct_return_data.ads', '-lit',
                      '-o', '../generated/wsdl_fct_return.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'wsdl_fct_return_types',
                      '-spec', 'wsdl_fct_return_data',
                      '-cb', 'wsdl_fct_return.wsdl'])

os.chdir('..')
build_and_run('wsdl_fct_return')
