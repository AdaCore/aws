
Files under this directory are provided to help building AWS with another
compiler than GNAT. GNAT users should not use these files.

Most packages have been edited to remove dependances to the GNAT
runtime. Features not used by AWS have been removed. See notes below.



GNAT
	Unmodifed.

GNAT.Regpat
	Procedure Dump (used for debugging purpose) has been removed.

GNAT.Regexp
	Do not use GNAT.Case_Util but equivalent routines from
	Ada.Characters.Handling.

GNAT.Calendar
	Type timeval removed, associated routines To_Duration and To_Timeval
	removed too.

GNAT.Calendar.Time_IO
	Unmodified.
