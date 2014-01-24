#
# To run manually to debug:
#
#   $ ada2wsdl -q -f -o api2.wsdl api2.ads
#   $ wsdl2aws -q -f -types api2 -cb api2.wsdl
#   $ gnatmake -g -O0 -Pcheck_mem_nossl
#   $ ./check_mem_nossl 2 > res1
#   $ gnatmem 5 -i gmem.out ./check_mem_nossl > run1
#   $ ./check_mem_nossl 5 > res2
#   $ gnatmem 5 -i gmem.out ./check_mem_nossl > run2

from test_support import *

#  Generate WSDL

exec_cmd('ada2wsdl', ['-q', '-f', '-o', 'api2.wsdl', 'api2.ads'],
         ignore_error=True)

#  Generate stub and skeleton

exec_cmd('wsdl2aws', ['-q', '-f', '-types', 'api2', '-cb', 'api2.wsdl'],
         ignore_error=True)

#  Build driver
build('check_mem_nossl')

#  Run driver (2 loops)
run('check_mem_nossl', ['2'])
exec_cmd('gnatmem', ['5', '-i', 'gmem.out', './check_mem_nossl'],
         output_file='check_mem_nossl.run1')

#  Run driver (10 loops)
run('check_mem_nossl', ['10'])
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

    print "run 1"
    for l in r1:
        print l

    print ""
    print "run 2"
    for l in r2:
        print l
