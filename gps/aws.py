
#  $Id$
#
#  AWS Python support for GPS integration
# 

import os, string, GPS

def locate_exec_on_path (prog):
    alldirs = string.split (os.getenv('PATH'), os.pathsep)
    for file in [os.path.join(dir,prog) for dir in alldirs]:
        if os.path.isfile(file) or os.path.isfile(file+".exe"):
            return file

def gnat_root ():
    gnatls = locate_exec_on_path ("gnatls")
    return gnatls[:-10]

def open_file (filename):
    file=GPS.File(filename)
    bfile=os.path.basename(file.name())
    if os.path.isfile(file.name()):
        GPS.EditorBuffer.get(file)
    elif os.path.isfile(GPS.get_system_dir() + "include/aws/" + bfile):
	GPS.Console ("Messages").write \
	  ("File " + bfile + " not part of project.")
	file=GPS.File(GPS.get_system_dir() + "include/aws/" + bfile)
        GPS.EditorBuffer.get(file);
    elif os.path.isfile(gnat_root() + "include/aws/" + bfile):
	GPS.Console ("Messages").write \
	  ("File " + bfile + " not part of project.")
	file=GPS.File(gnat_root() + "include/aws/" + bfile)
        GPS.EditorBuffer.get(file)
    else:
	GPS.Console ("Messages").write ("File " + bfile + " not found.")
