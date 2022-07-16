## Test environments
* local macOS R installation, R 4.1.2
* continuous integration via GH actions:
  * macOS-latest (release)
  * windows-latest (release)
  * ubuntu-latest (release)
  * ubuntu-latest (devel)
  * ubuntu-latest (oldrel-1)
* [win-builder](https://win-builder.r-project.org/) (release and devel)
* [R-hub](https://builder.r-hub.io)
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Debian Linux, R-devel, GCC ASAN/UBSAN

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

    * checking installed package size ... NOTE
      installed size is  8.4Mb
      sub-directories of 1Mb or more:
        doc   7.6Mb

The size varies according to the system on which the package is installed.
