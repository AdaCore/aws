from test_support import *

exec_cmd('webxref', ['wx_*'], output_file='wxrefs.res1')
diff('wxrefs.out1', 'wxrefs.res1')

exec_cmd('webxref', ['-u', 'wx_*'], output_file='wxrefs.res2')
diff('wxrefs.out2', 'wxrefs.res2')

exec_cmd('webxref', ['-d', 'wx_*'], output_file='wxrefs.res3')
diff('wxrefs.out3', 'wxrefs.res3')

exec_cmd('webxref', ['-I', '-C', '-pi', 'id', 'wx_*'],
         output_file='wxrefs.res4', ignore_error=True)
diff('wxrefs.out4', 'wxrefs.res4')

exec_cmd('webxref', ['-I', '-C', '-pi', 'file_based', 'wx_*'],
         output_file='wxrefs.res5', ignore_error=True)
diff('wxrefs.out5', 'wxrefs.res5')
