from test_support import *

files=['wx_block.thtml', 'wx_page.html', 'wx_reply.txml', 'wx_style.css']
exec_cmd('webxref', files, output_file='wxrefs.res1')
diff('wxrefs.out1', 'wxrefs.res1')

exec_cmd('webxref', ['-u'] + files, output_file='wxrefs.res2')
diff('wxrefs.out2', 'wxrefs.res2')

exec_cmd('webxref', ['-d'] + files, output_file='wxrefs.res3')
diff('wxrefs.out3', 'wxrefs.res3')

exec_cmd('webxref', ['-I', '-C', '-pi', 'id'] + files,
         output_file='wxrefs.res4', ignore_error=True)
diff('wxrefs.out4', 'wxrefs.res4')

exec_cmd('webxref', ['-I', '-C', '-pi', 'file_based'] + files,
         output_file='wxrefs.res5', ignore_error=True)
diff('wxrefs.out5', 'wxrefs.res5')
