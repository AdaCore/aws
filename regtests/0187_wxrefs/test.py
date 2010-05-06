from test_support import *

files=['wx_block.thtml', 'wx_page.html', 'wx_reply.txml', 'wx_style.css']
exec_cmd('webxref', files)
exec_cmd('webxref', ['-u'] + files)

exec_cmd('webxref', ['-d'] + files)

exec_cmd('webxref', ['-I', '-C', '-pi', 'id'] + files, ignore_error=True)

exec_cmd('webxref', ['-I', '-C', '-pi', 'file_based'] + files,
         ignore_error=True)
