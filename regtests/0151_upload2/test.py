from test_support import *

build('upload2')
run('upload2', output_file="res.out");

for item in open ('res.out').readlines():
    if item[0:13] != "Load address:":
        print item
