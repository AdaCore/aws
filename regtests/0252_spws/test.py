from test_support import *
from time import sleep

build('ws')
Server = Run(['./ws'], output='./server-output', bg=True)
sleep(1)

# websocket client application
from websocket import create_connection
ws = create_connection("ws://127.0.0.1:1234/server_push")

# Receiving 1 initial message and 5 server_push messages
for J in range(1, 7):
    result = ws.recv()
    print result

ws.close()
# check server output
res = Server.wait()
print "Server ends with status %d" % res
print "Server output:"
print open('server-output').read()
