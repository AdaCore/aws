import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-a', 'http://localhost:8787',
                      '-f', 'wsdl_char_schema_data.ads', '-lit',
                      '-o', '../generated/wsdl_char_schema.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-a', '-types', 'wsdl_char_schema_types',
                      '-spec', 'wsdl_char_schema_data',
                      '-cb', 'wsdl_char_schema.wsdl'])

os.chdir('..')
build_and_run('wsdl_char_schema')
