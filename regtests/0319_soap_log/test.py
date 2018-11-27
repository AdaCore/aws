from subprocess import Popen, PIPE
from test_support import exec_cmd, build
from time import sleep
import os.path

exec_cmd('ada2wsdl',
         ['-q', '-f', '-Pserver', '-o', 'srv.wsdl',
          '-doc', '-lit', '-a', 'http://localhost:7710', 'data.ads'])
exec_cmd('wsdl2aws',
         ['-q', '-f', '-cb', '-main', 'server',
          '-types', 'data', 'srv.wsdl'])

build('server')

server = Popen(["./server"], cwd=None, stdout=PIPE)

sleep(1)
exec_cmd('./client')

if os.path.exists("server.log"):
    print("OK server log found")
else:
    print("NOK server log not found")

if os.path.exists("server_error.log"):
    print("OK server error log found")
else:
    print("NOK server error log not found")

server.kill()
