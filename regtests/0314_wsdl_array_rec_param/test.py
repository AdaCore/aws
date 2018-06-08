import os
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-P', '../wsdl_array_rec_param.gpr',
                      '-a', 'http://localhost:9091',
                      '-f', 'aws_test.ads', '-doc', '-lit',
                      '-o', '../generated/aws_test.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-types', 'aws_test', '-cb', 'aws_test.wsdl'])

os.chdir('..')
build_and_run('wsdl_array_rec_param')
