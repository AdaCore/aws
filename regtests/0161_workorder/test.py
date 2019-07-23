from test_support import build, run
import re

build('workorder')
run('workorder', output_file="out.tmp")

for l in open('out.tmp').readlines():
    print(re.sub(r"\d\d\d\d-\d\d-\d\dT\d\d:\d\d:\d\d.\d\d\d([\+\-]\d\d:\d\d|Z)",
                 "XXXX-XX-XXTXX:XX:XXZ", l))
