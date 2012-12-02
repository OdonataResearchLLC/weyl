README FILE FOR SIMLAB R1 RELEASE
=================================
1. Overview of Release
----------------------
This release contains common lisp code for:
  a)the Weyl computer algebra substrate,
  b)creating guaranteed-quality triangulations of planar areas.


2. Copyright / Acknowledgements / Disclaimer
--------------------------------------------
All code is copyrighted Cornell University, 1995.

Code has been developed by Paul Chew, Paul Jackson, Shekar Muddana,
Rick Palmer, Todd Wilson and Richard Zippel in the Simlab group at
Cornell University.

This work was supported in part by the Advanced Research Projects
Agency of the Department of Defense under ONR Contract
N00014--92--J--1989, by ONR Contract N00014--92--J--1839, and in part
by the U.S. Army Research Office through the Mathematical Science
Institute of Cornell University.

This material is NOT Public Domain, but permission to copy this
software, to redistribute it, and to use it for any purpose is
granted, subject to the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the Cornell Modeling and Simulation Project any improvements
or extensions that they make, so that these may be included in future
releases; and (b) to inform Cornell of noteworthy uses of this
software.

3. All redistributions of this software must include the sources,
machine readable documentation and any other machine readable material
provided as part of this distribution by Cornell in full.

4. All materials developed as a consequence of the use of this software
shall duly acknowledge such use, in accordance with the usual standards of
acknowledging credit in academic research.

5. Cornell University has made no warrantee or representation that
the operation of this software will be error-free, and Cornell
University is under no obligation to provide any services, by way of
maintenance, update, or otherwise.

6. In conjunction with products arising from the use of this
material, there shall be no use of the name of Cornell University
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from Cornell in each
case.


3. System requirements
----------------------

The Lisp code in this release conforms to the ANSI Common Lisp
specification.  It has been tested using Lucid common Lisp Version
4.1 and Allegro Common Lisp Version 4.2beta2.0 on Sun Sparstations and on
Apple Macintosh computers running Apple Common Lisp version 2.01.


4. Contents
-----------
The release consists of two files:
  a) README: this file
  b) simlab.tar.gz : a gzipped tarfile of the directory tree.

The directory tree has the following main structure:

  notes/               
  registry/            ; lisp defsystem files.
  weyl/
  weyl/src/            ; lisp code for Weyl and mesher.
  weyl/doc/manual/     ; manual.
  weyl/demo/           ; demo files.
  utilities/lisp/      ; lisp utilities (inc sources for defsystem).
  gfx/src              ; lisp code for displaying complexes and chains 
                         (uses clx).  

5. Installation
---------------
(on Unix systems)

To install the lisp files, follow the directions a), b), c) below.
Then follow 6. each time you want to use Weyl or the meshing system.

a) Unpack the tar file: create a new directory for the Simlab files,
   place the simlab.tar.gz file there, cd to that directory, and execute 
   the Unix command:

     gunzip simlab.tar.gz
     tar xf simlab.tar

b) Configure your Lisp environment for the DEFSYSTEM utility that
   the Simlab Lisp files use:

   (This utility is a Lisp version of the Unix `make'; it allows for easy 
    loading / compiling of the Simlab Lisp files)

   Follow directions in the file `notes/lisp-setup.txt'.

   Note that the first use of the utility causes a couple of Lisp files to 
   be compiled. This compilation sometimes generates warning messages that
   can be safely ignored.
 
c) Compile all the Lisp files: 

   Start up your Lisp and enter at the Lisp prompt:

   (compile-system 'weyl)


6. Startup
----------
(Assuming Simlab installed as described in Section 5).

Start up your Lisp and enter at the Lisp prompt:

   (load-system 'weyl)


7. Documentation and Examples
-----------------------------
The postscript form of a comprehensive manual is available in file 

  weyl/doc/manual/manual.ps


This manual gives many examples of the use of Weyl and the meshing code.
In addition, examples may be found in the demo files in directory weyl/demo.

7. Feedback / comments
----------------------
Feedback, comments and bug reports should be addressed to

  simlab@cs.cornell.edu

or to:

Simlab Project Coordinator
Department of Computer Science
Cornell University 
Ithaca NY 14853
USA

