## Notes

### Fifth submission

>Please omit the redundant "in R" at the end of your title.

Changed the title to "A Set of Tools For Sound Localization".

>Please omit the space within the doi specification to make it clickable.

Done.

>Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar)
>Missing Rd-tags:
>      locHeatmap.Rd: \value
>      omniSpectro.Rd: \value
>      validationSpec.Rd: \value

Added "@return No return value." to these functions.

>You have examples for unexported functions. Please either omit these
examples or export these functions.
>Examples for unexported function
>   MSRP_HT_Level2() in:
>      parseWAFileNames.Rd

Deleted the example in parseWAFileNames.Rd. Note: the function MSRP_HT_Level2() 
is located in a different file and does not have an example, as far as I can see.

>\dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
>Please replace \dontrun with \donttest. -> parseWAFileNames.Rd

This example has been removed from the package since the function is not exported (see previous bullet).

>Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.

All examples wrapped executed in >5 sec (the exception was the example above, which has been removed). All that are wrapped use \donttest{} rather than \dontrun{}. 

>You write information messages to the console that cannot be easily
suppressed.
>It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of print() rather use message()/warning() or
if(verbose)print(..) (or maybe stop()) if you really have to write text
to the console. (except for print, summary, interactive functions) ->
R/omniSpectro.R

Replaced print() with message() in omniSpectro.R.

>Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited.
>e.g.: -> R/localize.R; R/omniSpectro.R
>...
>oldpar <- par(no.readonly = TRUE) # code line i
>on.exit(par(oldpar)) # code line i + 1
>...
>par(mfrow=c(2,2)) # somewhere after
>...

>e.g.:
>If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.

Added on.exit() call as follows, immediately prior to the par() calls:

    oldpar <- par(no.readonly = TRUE)

    on.exit(par(oldpar), add = TRUE)

>Please always make sure to reset to user's options(), working directory
or par() after you changed it in examples and vignettes and demos. ->
man/validationSpec.Rd
>e.g.:
>oldpar <- par(mfrow = c(1,2))
>...
>par(oldpar)

Added code to reset par() for the user, as follows:

    oldpar <- par()$mfrow

    ...

    par(mfrow = oldpar)

### Fourth submission

>The Description field should not start with the package name, 'This package' or similar.

Fixed now, I misunderstood the comment before.

### Third submission

>Found the following (possibly) invalid file URI:
URI: Intro_To_locaR.html
From: inst/doc/V3_Intro_to_localize.html

This has been fixed by removing the link, and simply referring to the other vignette by name (I only fixed one instance in the second submission, and have now fixed the other instance in the same way).

>The Description field should not start with the package name, 'This package' or similar.

As far as I can see, the Description field starts with "This package", but maybe I am missing something.

### Second submission

>Found the following (possibly) invalid file URI:
URI: Intro_To_locaR.html
From: inst/doc/V3_Intro_to_localize.html

This has been fixed by removing the link, and simply referring to the other vignette by name.

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


