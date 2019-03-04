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

EXEC='check_mem_nossl'

#  Build driver
build(EXEC)

first_high=False
first_final=False

#  Run driver 2 times
for r in range(0, 2):
    run(EXEC, [str(r * 8 + 2)])
    ofn = open("run%d.out" % r, "w+");
    exec_cmd('gnatmem', ['5', '-t', '-i', 'gmem.out', EXEC],
              output_file=ofn)
    ofn.seek(0)
    for line in ofn:
       if line[0:8] == "   Final":
           if first_final:
               if first_final == line:
                   print "OK final water mark"
               else:
                   print first_final + line
           else:
              first_final = line

       elif line[0:7] == "   High":
           if first_high:
               if first_high == line:
                   print "OK high water mark"
               else:
                   print first_high + line
               break
           else:
              first_high = line

    ofn.close()
