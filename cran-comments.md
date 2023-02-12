## Notes

### Second submission

>Found the following (possibly) invalid file URI:
URI: Intro_To_locaR.html
From: inst/doc/V3_Intro_to_localize.html

This has been fixed.

>The Description says
Description: Sound localization in R.
Can you please elaborate which methods are implemented? Is there some
reference about the method you can add in the Description field in the
form Authors (year) <doi:10.....> or <arXiv:.....>?

We have added a reference for Cobos et al. (2010), which is the method used in the package.

## Test environments
* local Windows 10 install, R-devel
* rhub check_for_cran
* win-builder

## R CMD check results (local Windows 10 install)
0 ERRORs, 0 WARNINGs, 0 NOTEs.  

## rhub check_for_cran (Windows Server 2022, R-devel, 64 bit)
[https://builder.r-hub.io/status/locaR_0.1.2.tar.gz-5b9f84929d4348648621d2a0c75d36ce](https://builder.r-hub.io/status/locaR_0.1.2.tar.gz-5b9f84929d4348648621d2a0c75d36ce)

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
[https://builder.r-hub.io/status/locaR_0.1.2.tar.gz-b6ae975c951f4daa8d5ed18c550f09a8](https://builder.r-hub.io/status/locaR_0.1.2.tar.gz-b6ae975c951f4daa8d5ed18c550f09a8)

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
[https://builder.r-hub.io/status/locaR_0.1.2.tar.gz-1656b198d756493aa8e2f68ffa8d41f7](https://builder.r-hub.io/status/locaR_0.1.2.tar.gz-1656b198d756493aa8e2f68ffa8d41f7)

0 ERRORs, 0 WARNINGS, 1 NOTE.
```
* checking CRAN incoming feasibility ... [6s/24s] NOTE
Maintainer: ‘Richard Hedley <rwhedley@gmail.com>’
New submission
```

## win-builder
0 ERRORs, 0 WARNINGS, 1 NOTE.
```
* checking CRAN incoming feasibility ... [8s] NOTE
Maintainer: 'Richard Hedley <rwhedley@gmail.com>'
New submission
```


