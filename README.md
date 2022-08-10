# Parallelization

Here you have two programs, that do the same task, but one of those
use parallelization. The way to compile which one is:

`gfortran power_method_standar.f90 -o std.exe`

`pgfortran -acc -Minfo power_method_parallel.f90 -o parallel.exe`

To write a big matrix there are also a dummy program to do it.
