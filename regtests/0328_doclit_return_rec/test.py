from test_support import exec_cmd, build, run
import re

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Ptest', '-o', 'test.wsdl', '-doc', '-lit',
          '-a', 'http://localhost:8212', 'test_aws.ads'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-debug', '-cb', '-spec', 'test_aws',
          '-types', 'types_aws', 'test.wsdl'])

build('test')
run('test', output_file='res.out')

s1 = re.compile('.*Result_Test3.*')
s2 = re.compile('.*Test3_Result.*')
s3 = re.compile('.*xmlns:n1.*')
s4 = re.compile('.*xmlns:n2.*')

out = open ('res.out').readlines()

# output
for item in out:
    if item[0:1] == "@":
        print(item)

# name space
for item in out:
    if s3.match(item):
        print(item.strip("<> \r\n"))
        break
for item in out:
    if s4.match(item):
        print(item.strip("<> \r\n"))
        break

# xml payload
for item in out:
    if s1.match(item) or s2.match(item):
        print(item)
