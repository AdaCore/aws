import sys
import time

from test_support import *

output = open('test.res', 'w')
output.write(time.strftime("%y-%m-%d %H:%M", time.localtime()))
output.close()
diff()

sys.exit(1)
