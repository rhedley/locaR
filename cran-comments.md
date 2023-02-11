## Test environments
* local Windows 10 install, R-devel
* rhub check_for_cran

## R CMD check results (local Windows 10 install)
0 ERRORs, 0 WARNINGs, 0 NOTEs.  

## rhub check_for_cran (Windows Server 2022, R-devel, 64 bit)
0 ERRORs, 0 WARNINGS, 2 NOTEs.
```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Richard Hedley <rwhedley@gmail.com>’
New submission
```
```
* checking for detritus in the temp directory ... NOTE Found the following files/directories:
'lastMiKTeXException'
```

As noted in [R-hub issue #503](https://github.com/r-hub/rhub/issues/503), this could be due to a bug/crash in MiKTeX and can likely be ignored.

## rhub check_for_cran (Fedora Linux, R-devel, clang, gfortran)
0 ERRORs, 0 WARNINGS, 2 NOTEs.

```
* checking CRAN incoming feasibility ... [6s/24s] NOTE
Maintainer: ‘Richard Hedley <rwhedley@gmail.com>’
New submission
```
```
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
```
According to [this stackexchange answer](https://stackoverflow.com/questions/74857062/rhub-cran-check-keeps-giving-html-note-on-fedora-test-no-command-tidy-found), this is an issue with the test platform, not the package.

## rhub check_for_cran (Ubuntu Linux 20.04.1 LTS, R-release, GCC)
0 ERRORs, 0 WARNINGS, 1 NOTE.
```
* checking CRAN incoming feasibility ... [6s/24s] NOTE
Maintainer: ‘Richard Hedley <rwhedley@gmail.com>’
New submission
```




