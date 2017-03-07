import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-a', 'http://localhost:8787',
                      '-f', 'gen_datetime.ads',
                      '-o', '../generated/gen_datetime.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', 'gen_datetime.wsdl'])

os.chdir('..')
build('gen_datetime')
