import os
import re
from test_support import *

os.mkdir('generated')
os.chdir('src')

exec_cmd('ada2wsdl', ['-q', '-P', '../api.gpr', '-a', 'http://localhost:8787',
                      '-f', 'rpc_array_record_data.ads',
                      '-o', '../generated/rpc_array_record.wsdl'])

os.chdir('../generated')

exec_cmd('wsdl2aws', ['-q', '-debug', '-a', '-types', 'rpc_array_record_types',
                      '-spec', 'rpc_array_record_data',
                      '-cb', 'rpc_array_record.wsdl'])

os.chdir('..')
build('rpc_array_record')
exec_cmd('./rpc_array_record', output_file='tmp.run')

r = open ('tmp.run').readlines()

s = re.compile('.*soapenc:arrayType="([^"]+).*')

for item in r:
    m = s.match(item)
    if item[0] == '@':
        print item
    if m:
        print m.group(1)
