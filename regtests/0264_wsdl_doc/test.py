from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'wsdl_doc.wsdl'])
build_and_run('wsdl_doc')

def output_comment(file):
    print file
    lines = open(file).readlines()
    for l in lines[8:]:
        if l[0:3] == "-- " or l[6:9] == "-- " or l[3:6] == "-- ":
            print l

for file in ["r_hello_demo-client.ads",
             "r_hello_demo-server.ads",
             "www-s-com-wsdl-hs_wsdl-arrayoffloat_type_pkg.ads",
             "www-s-com-wsdl-hs_wsdl-helloresponse_type_pkg.ads"]:
    output_comment (file)
