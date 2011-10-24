from test_support import *

build('content_type_data_range');
exec_cmd('content_type_data_range', output_file='result.txt')

r = open ('result.txt').readlines()

for item in r:
    if item[0:14] == ">Content-Type:":
        print item
