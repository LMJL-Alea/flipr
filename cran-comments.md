## Resubmission
This is a resubmission. In this version I have:

* Reduced the size of the tarball to fall below the 5Mb requirement by replacing a number of [**plotly**](https://plotly.com/r/) contour plots with their [**ggplot2**](https://ggplot2.tidyverse.org) counterparts.
  
## Test environments
* local macOS R installation, R 4.0.3
* macOS latest release (via [R-CMD-check](https://github.com/r-lib/actions/blob/master/examples/check-standard.yaml) github action)
* windows latest release (via [R-CMD-check](https://github.com/r-lib/actions/blob/master/examples/check-standard.yaml) github action)
* ubuntu 20.04 latest both release and devel (via [R-CMD-check](https://github.com/r-lib/actions/blob/master/examples/check-standard.yaml) github action)
* [win-builder](https://win-builder.r-project.org/) (release and devel)
* [R-hub](https://builder.r-hub.io)
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Oracle Solaris 10, x86, 32 bit, R-release

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

    * checking installed package size ... NOTE
      installed size is  8.0Mb
      sub-directories of 1Mb or more:
        doc   7.7Mb

The size varies according to the system on which the package is installed.
