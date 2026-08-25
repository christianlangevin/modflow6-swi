README                                                         July 26, 2004

                                   SHARP

        A quasi-three-dimensional, numerical finite-difference model 
          to simulate freshwater and saltwater flow separated by 
           a sharp interface in layered coastal aquifer systems

                      SHARP - Version 1.1  1999/05/10

Instructions for installation, execution, and testing are provided
below.  After installation, see the file doc\sharp.txt in the SHARP
installation directory for summary information on SHARP, including
documentation references.  For assistance, enhancement requests, or to
report bugs, contact the Hydrologic Analysis Software Support Program by
sending e-mail to h2osoft@usgs.gov.

For all versions of Microsoft Windows, this DOS version of SHARP must be
installed and run using an MS-DOS Command Prompt window.

                            TABLE OF CONTENTS

                         A. SYSTEM REQUIREMENTS
                         B. DISTRIBUTION FILES
                         C. INSTALLING
                             o Installing SHARP
                             o SHARP directory structure
                             o Making SHARP easily accessible
                         D. COMPILING
                         E. RUNNING THE SOFTWARE
                         F. TESTING


A. SYSTEM REQUIREMENTS

For installation of SHARP, 845 kilobytes of free disk space is needed.

To run SHARP, the following are necessary:

  - 386 or greater processor
  - math coprocessor
  - 4.2 megabytes of combined free extended memory and free disk space
    on installation drive (the greater proportion available as memory,
    the better performance will be)


B. DISTRIBUTION FILES

The following self-extracting DOS distribution files (containing the
software, test data sets, and information files) are currently
available.

  shrp1_1.exe  - Compiled using Compaq Visual Fortran (with source code)
  shrp1_1b.exe - Compiled using Compaq Visual Fortran (binaries only)
  shrp1_1s.exe - Source code only


C. INSTALLING

Installing SHARP
-----------------

These instructions assume that you are installing the software in
directory c:\wrdapp, which we strongly recommend.  You may use other
directories or disk drives, but you must change each occurrence of "c:"
and "\wrdapp" in all of the following instructions to the alternate
location that you choose.

Follow the steps below to install SHARP from a distribution file.  The
steps make use of DOS commands which, for all versions of Microsoft
Windows, must be executed in an MS-DOS Command Prompt window.

   1. Move to the directory containing the distribution file; for
      example:

        c:
        cd \mydir

      The directory sharp1.1 is created (or overwritten) upon extracting
      the files contained in the distribution file.  If the sharp1.1
      directory already exists, you may want to delete or rename it
      before performing the next step.

   2. Extract the files using the command:

        shrp1_1 -d -o c:\wrdapp

      Substitute "shrp1_1b" or "shrp1_1s" for "shrp1_1" if you are
      installing the executable-code-only or source-code distributions,
      respectively.  Note, be sure to include the -d (restore directory
      structure) and -o (overwrite existing files) options.


SHARP directory structure
--------------------------

The following directory structure will be created (the contents of each
directory are shown to the right):

   sharp1.1             copy of this README file
     `-----bin          compiled executable
     `-----doc          documentation file
     `-----src          makefile and source code
     `-----test         batch files to run verification tests
     `-----data         standard data sets used in verification tests

Notes:
a) The bin directory is not included in the shrp1_1s.exe distribution file
   (it should be created after compilation and the compiled executable 
   should be placed in it).
b) The source code is not included in the shrp1_1b.exe distribution file.
c) It is recommended that no user files be kept in the sharp1.1
   directory structure.  If you plan to put files in the sharp1.1
   directory structure, do so only by creating subdirectories
   under sharp1.1.


Making SHARP easily accessible
-------------------------------

To make the SHARP program accessible from any directory, without needing
to type the full pathname of the software's location, the sharp1.1\bin
directory must be included in the PATH environment variable.

On Windows 3.x and Windows 9x systems, add the following line to the end
of AUTOEXEC.BAT:

  PATH=%PATH%;c:\wrdapp\sharp1.1\bin

Reboot your system after modifying AUTOEXEC.BAT.

On Windows NT systems, from the Start menu select Settings and then
Control Panel.  Double-click System and select the Environment tab.
Enter "PATH" in the Variable field and enter
"%PATH%;c:\wrdapp\sharp1.1\bin" in the Value field.  Click Set and then
click OK.  Initiate and use a new MS-DOS Command Prompt window after
making this change.

For other operating systems, check the online help for the operating system
on how to specify an Environment variable.

D. COMPILING

The source code is provided in the shrp1_1.exe and shrp1_1s.exe
distribution files so that users can generate the executable themselves.
No support can be provided for users generating their own versions of the
software.  In general, the requirements are a Fortran compiler and a
minimal level of knowledge of the compiler and the operating system.

E. RUNNING THE PROGRAM

After the SHARP executable is properly installed (see INSTALLING, above),
the program can be executed with the command "sharp".  Input data for
the SHARP model is read from the file named "INPUT".  Output is
written to predefined output file names.

SHARP has been compiled using the Compaq Visual Fortran Professional Edition 6.5.0.


F. TESTING

Test data sets are provided to verify that the program is correctly
installed and running on the system.  The tests may also be looked at as
examples of how to use the program.  The directory sharp1.1\test
contains the batch files to run the tests.  The directory sharp1.1\data
contains the input data and expected results for each test.  Tests are
usually run in the directory sharp1.1\test, but they can be run in any
user directory by assigning the SHARP installation directory
(c:\wrdapp\sharp1.1) to the "topdir" variable in
sharp1.1\test\test.bat.

To test the installation, change to the sharp1.1\test directory and type
the command:
      test [m [n]]  

If running from another directory (provided that the "topdir" variable in
test.bat was modified appropriately), specify the full path to the batch
file; for example:
      c:\wrdapp\sharp1.1\test\test [m [n]]

where:
     m = the number of the first test to perform, default=1
     n = the number of the last test to perform, default=2

For example:

command                         what happens
------------------      ------------------------------------
test                    runs all of the tests
test n                  runs test 'n' through the last test
test n m                runs test 'n' through 'm'

After the tests are completed, the results are compared to the expected
results.  If all goes well, there should be no differences.  To clean up
after the tests, with sharp1.1\test as the current working directory, 
type the command:
      clean

The tests are described in the table below.  Test is the test number and
the usage column indicates how a file is used, with i for input and o for
output.

Note that file name suffixes of "t1" and "t2" have been added to the input
and output files to indicate the test to which each set of files
corresponds; the input and output file names do not normally bear
suffixes.


test  description of test and files               file name & usage 
----  -------------------------------------       ----------------- 
  1   Areal model of SE Oahu 

      SHARP input file                            INPUT.t1       i
      summary of processed data                   OUTPUT.t1      o
      final head and interface data               RESULTS.t1     o
      observation location heads                  HEADS.t1       o
      grid block leakages                         LEAK.t1        o

 
  2   Cape May cross-sectional model 

      SHARP input file                            INPUT.t2       i
      summary of processed data                   OUTPUT.t2      o
      final head and interface data               RESULTS.t2     o
      observation location heads                  HEADS.t2       o
      grid block leakages                         LEAK.t2        o
