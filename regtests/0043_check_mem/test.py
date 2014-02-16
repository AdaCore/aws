from test_support import *

#  Build driver
build('check_mem')

#  Run driver (2 loops)
run('check_mem', ['2'])
exec_cmd('gnatmem', ['5', '-i', 'gmem.out', './check_mem'],
         output_file='check_mem.run1')

#  Run driver (10 loops)
run('check_mem', ['10'])
exec_cmd('gnatmem', ['5', '-i', 'gmem.out', './check_mem'],
         output_file='check_mem.run2')

#  Now check that final water-mark for run1 and run2 is equal
r1 = open ('check_mem.run1').readlines()
r2 = open ('check_mem.run2').readlines()

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
