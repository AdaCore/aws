from test_support import *

#  Generate WSDL

exec_cmd('ada2wsdl', ['-q', '-f', '-o', 'api.wsdl', 'api.ads'],
         ignore_error=True)

#  Generate stub and skeleton

exec_cmd('wsdl2aws', ['-q', '-f', '-types', 'api', '-cb', 'api.wsdl'],
         ignore_error=True)

#  Build driver
build('check_mem_nossl')

#  Run driver (2 loops)
run('check_mem_nossl', ['2'], output_file='check_mem_nossl.res1')
exec_cmd('gnatmem', ['5', '-i', 'gmem.out', './check_mem_nossl'],
         output_file='check_mem_nossl.run1')

#  Run driver (10 loops)
run('check_mem_nossl', ['10'], output_file='check_mem_nossl.res2')
exec_cmd('gnatmem', ['5', '-i', 'gmem.out', './check_mem_nossl'],
         output_file='check_mem_nossl.run2')

#  Now check that final water-mark for run1 and run2 is equal
r1 = open ('check_mem_nossl.run1').readlines()
r2 = open ('check_mem_nossl.run2').readlines()

fr1 = "1"
fr2 = "2"

for item in r1:
    if item[0:8] == "   Final":
        fr1 = item;
for item in r2:
    if item[0:8] == "   Final":
        fr2 = item;

if fr1 != fr2:
    logging.error(fr1 + "!=" + fr2)
