## Test environments

* local OS MS install, R 4.0.3
* Continuous Integration
  * GitHub actions (ubuntu-20.04): release, devel
  * GitHub actions (windows): release
  * Github actions (OS X): release
* Rhub
  * Debian Linux, R-devel, GCC ASAN/UBSAN
  * Fedora Linux, R-devel, clang, gfortran
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.


**Comment:** "The Description field should not start with the package name, 'This package' or similar.""
Please fix and resubmit.

The description now does **NOT** start with "The package ...".

**Comment:** Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:.....>?

Several references are mentioned in the functions; however, the package does not focus on a single methodology, so there is no reference that can be singled-out and included in the description.
