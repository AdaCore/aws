from test_support import *

build('unexph2')
run('unexph2', output_file="res.out");

for item in open ('res.out').readlines():
    if item[0:13] != "Load address:":
        print item
