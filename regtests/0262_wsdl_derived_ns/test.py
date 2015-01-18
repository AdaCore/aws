from test_support import *

exec_cmd('wsdl2aws', ['-f', '-doc', '-v', 'wsdl_derived.wsdl'],
         ignore_error=True)
build('wsdl_derived')

P1 = "urn-whatever-different-"
U1 = P1 + "nonnegativeint_type_pkg.ads"

P2 = "www-ecerami-com-wsdl-aliasservice_wsdl-"
U2 = P2 + "percentcompleteinteger_type_pkg.ads"

if os.path.exists(U1):
    print("OK nonnegativeint")
else:
    print("NOK nonnegativeint")

if os.path.exists(U2):
    print("OK percentcompleteinteger")
else:
    print("NOK percentcompleteinteger")
