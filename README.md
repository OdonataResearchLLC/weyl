## Weyl Computer Algebra Substrate

This repository contains an updated version of the [Weyl computer
algebra substrate][WEYL] from [Cornell University][Cornell]. It is
[ASDF][] loadable, but is currently limited to [SBCL][]. It will be
updated to compile and load on other implementations. The original
sources were extracted from the [SimLab release 1-0][SimLab].

Weyl is an extensible algebraic manipulation substrate that has been
designed to represent all types of algebraic objects. It deals not
only with the basic symbolic objects like polynomials, algebraic
functions and differential forms, but can also deal with higher level
objects like groups, rings, ideals and vector spaces. Furthermore, to
encourage the use of symbolic techniques within other applications,
Weyl is implemented as an extension of Common Lisp using the Common
Lisp Object Standard so that all of Common Lisp's facilities and
development tools can be used in concert with Weyl's symbolic tools.

[WEYL]: https://www.cs.cornell.edu/rz/computer-algebra.html
[Cornell]: https://www.cs.cornell.edu/
[ASDF]: http://common-lisp.net/project/asdf/
[SBCL]: http://sbcl.sourceforge.net/
[SimLab]: https://www.cs.cornell.edu/Info/Projects/SimLab/releases/release-1-0.html

### System requirements

* SBCL
* ASDF

### Installation

* Clone the Weyl repository
* Compile and load with ASDF : `(asdf:load-system :weyl)`

### Documentation and Examples

The PDF form of a comprehensive manual is available in file 

  weyl/reference/Weyl Manual.pdf

This manual gives many examples of the use of Weyl and the meshing code.

### Tasks

Scheduled

* (0.2.0) Write unit tests based on the examples in the manual.
* (0.3.0) Replace SBCL specific code and test on other
  implementations.

Unscheduled

* Implement custom conditions.
* Better separate the definitions in WEYL from WEYLI. It would be nice
  to have a WEYL-KERNEL, WEYL, and a WEYL-USER package.

The WEYL-KERNEL package would form the core routines for WEYL and
WEYL-USER. The WEYL package would be used by other packages. The
WEYL-USER package would be for interactive use. This approach would
hopefully negate the necessity for `use-weyl-package`.

### Copyright / Acknowledgements / Disclaimer

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
