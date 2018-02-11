

# R package "fguigb"

The main function exported from this package takes a function name and creates a
GUI window, such that the user can try various values for the arguments, see the
command that will be run, and run it on exit. For each argument, its help text
from the documentation page of the function is provided.  The GUI window is
created with functions from Thomas Hoffmann's package `fgui`, modified to work
with the new Rd format introduced circa 2009/2010.

