import os
from test_support import *

os.mkdir('generated')
os.mkdir('obj')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-Pdefault', '-a', 'http://localhost:8787',
                      '-f', 'wsdl_array_record_data.ads',
                      '-o', '../generated/wsdl_array_record.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'wsdl_array_record_data',
                      '-cb', 'wsdl_array_record.wsdl'])

os.chdir('..')
build('wsdl_array_record')
