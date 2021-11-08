from test_support import *

build('content_type_data_range')
run('content_type_data_range', output_file='result.txt')

r = open('result.txt').readlines()

for item in r:
    item = item.lower()
    if item.find("content-") >= 0 and item != ">content-type: text/plain\n":
        print item
