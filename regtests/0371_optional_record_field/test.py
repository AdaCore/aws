from test_support import *

os.chdir('generated')

exec_cmd('wsdl2aws', ['-v', '-f', 'service.wsdl'],
         output_file='wsdl2aws.res')

os.chdir('..')

build_and_run('m_minoccurs')
