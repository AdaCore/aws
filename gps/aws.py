
#
#  AWS Python support for GPS integration
#

import os, string, GPS

def locate_exec_on_path (prog):
    alldirs = string.split (os.getenv('PATH'), os.pathsep)
    for file in [os.path.join(dir,prog) for dir in alldirs]:
        if os.path.isfile(file) or os.path.isfile(file+".exe"):
            return file

class Console_Process (GPS.Console, GPS.Process):
    out_file = ""

    def on_output (self, unmatched, matched):
        self.write (unmatched + matched)

    def on_input (self, input):
        self.send (input)

    def on_exit (self, status, remaining_output):
	if status == 0 and self.out_file != "":
	   GPS.Editor.edit (self.out_file, force=True)

    def __init__ (self, ofile, process, args=""):
	self.out_file = ofile
        GPS.Console.__init__ (self, process, \
                              on_input=Console_Process.on_input)
        GPS.Process.__init__ (self, process + ' ' + args, ".+", \
			      on_exit=Console_Process.on_exit, \
                              on_match=Console_Process.on_output)

# For API documentation support

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
	  ("File " + bfile + " is not part of current project.\n")
	file=GPS.File(GPS.get_system_dir() + "include/aws/" + bfile)
        GPS.EditorBuffer.get(file);
    elif os.path.isfile(gnat_root() + "include/aws/" + bfile):
	GPS.Console ("Messages").write \
	  ("File " + bfile + " is not part of current project.\n")
	file=GPS.File(gnat_root() + "include/aws/" + bfile)
        GPS.EditorBuffer.get(file)
    else:
	GPS.Console ("Messages").write ("File " + bfile + " not found.\n")

# For SOAP support

def ada2wsdl (api):
    toolexe=locate_exec_on_path("ada2wsdl");
    if toolexe == "":
        GPS.Console ("Messages").write ("Error: ada2wsdl tool not found.");
    else:
        ofile=os.path.splitext(api)[0]+'.wsdl'
    	idirs=[];
    	proj=GPS.current_context().project();
    	for dir in proj.source_dirs():
            idirs = idirs + ["-I", dir];
	opts = GPS.Project.get_tool_switches_as_list (proj, "Ada2WSDL") \
	    + ["-o", ofile] + idirs + [api];
        fopts=" ".join(opts); fopts=string.replace(fopts, "\\", "/");
        Console_Process (ofile, toolexe, fopts);

def wsdl2aws (wsdl):
    toolexe=locate_exec_on_path("wsdl2aws");
    if toolexe == "":
        GPS.Console ("Messages").write ("Error: wsdl2aws tool not found.");
    else:
        proj=GPS.current_context().project();
        opts = GPS.Project.get_tool_switches_as_list (proj, "WSDL2AWS") \
	         + [wsdl];
        fopts=" ".join(opts);
    	Console_Process ("", toolexe, fopts);
