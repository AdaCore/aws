from test_support import *

exec_cmd('wsdl2aws', ['-v', '-f', '-doc', 'wsdl_choice.wsdl'],
         ignore_error=True)

build("wsdl_choice")

if os.path.exists("urn-naws-wsdl_choice-status_type_pkg.ads"):
    print("OK")
else:
    print("NOK")
