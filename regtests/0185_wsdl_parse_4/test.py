from test_support import *

exec_cmd('wsdl2aws', ['-q', '-f', '-doc', 'wsdl_parse_4.wsdl'],
         ignore_error=True)

if os.path.exists("urn-naws-wsdl_4-character_type_pkg.ads"):
    print("OK")
else:
    print("NOK")
