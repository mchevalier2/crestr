---
title: "cran-comments"
author: "Manuel Chevalier"
date: "03/11/2021"
output: github_document
---

# Re-submission

- I have now replaced all the http by https.
- The webpage (https://journals.ametsoc.org/view/journals/clim/20/22/2007jcli1824.1.xml) still seems to create some issues with windows builts, but I have no idea how to fix this. So I have replaced it by an alternative one.
- This other address (https://www.ncei.noaa.gov/products/world-ocean-atlas) seems to _sometimes_ create problems (Message: libcurl error code 35:). Sometimes it passes the tests, sometimes it doesn't. Since I really don't know much in this area, I have no idea how to fix this. If you have any suggestion on how to fix this, please let me know how. Otherwise, it will have to stay as it is since there is no alternative link I can use.



# First submission

`crestr 1.0.0` is a package designed to estimate climate parameters from fossil bio-prxoies. Its application will primarily (and uniquely?) for academic purposes.

# Test environments

* local OS X install, 11.5.2, R  4.0.2
* win-builder, using `devtools::check_win_devel()`, `devtools::check_win_release()` and `devtools::check_win_oldrelease()`
* r-hub (windows, Ubuntu and Fedora)


## Local build

There were no ERRORs, WARNINGs or NOTES.

* Package built by
```{r eval = FALSE}
devtools::build(manual = TRUE)
```

* Package checked by
```{bash eval=FALSE}
R CMD CHECK /Users/mchevali1/GitHub/Rpackages/crestr_1.0.0.tar.gz
Status: OK
```

* Package checked by
```{bash eval=FALSE}
R CMD CHECK --as-cran /Users/mchevali1/GitHub/Rpackages/crestr_1.0.0.tar.gz
Status: OK
Notes: New Submission
```

## WIN_DEVEL

There were no ERRORs or WARNINGs. In all three cases, there was 1 NOTE because this is a first submission.

* `devtools::check_win_devel()` resulted in:
```
Status: 1 NOTE
> checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Manuel Chevalier <chevalier.manuel@gmail.com>'
> New submission
```

* `devtools::check_win_release()` resulted in:
```
Status: 1 NOTE
> checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Manuel Chevalier <chevalier.manuel@gmail.com>'
> New submission
```

* `devtools::check_win_oldrelease()` resulted in:
```
Status: 1 NOTE
> checking CRAN incoming feasibility ... NOTE
> Maintainer: 'Manuel Chevalier <chevalier.manuel@gmail.com>'
> New submission
```

I also receive this message, about possibly invalid URL links in my documentation? These links have been tested and are all valid.
```
Found the following (possibly) invalid URLs:
  URL:
    From: inst/doc/technicalities.html
    Message: Empty URL
  URL: http://www.gbif.org (moved to https://www.gbif.org/)
    From: inst/doc/calibration-data.html
    Status: 200
    Message: OK
  URL: https://doi.org/10.1175/2007JCLI1824.1
    From: inst/doc/calibration-data.html
    Status: 403
    Message: Forbidden
  URL: https://figshare.com/articles/GBIF_for_CREST_database/6743207 (moved to https://figshare.com/articles/dataset/GBIF_for_CREST_database/6743207)
    From: inst/doc/calibration-data.html
    Status: 200
    Message: OK
```


## R-HUB

Here, I have checked the package using five tests from the `rhub` package. The check_on_linux(), check_on_windows(), check_on_fedora() and check_with_rrelease() tests returned no ERRORs, WARNINGs or NOTEs. The check_for_cran() tests returned no ERRORs or WARNINGs but consistently returned 1 NOTE about this being a `New Submission.


### `check_for_cran()`


```{r eval=FALSE}
rhub::check_for_cran()
```

The result is:

```
Build ID:	crestr_1.0.0.tar.gz-30e2d9b176a94f78a99daadacb217642
Platform:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit
Submitted:	4 minutes 40.5 seconds ago
Build time:	4 minutes 38.3 seconds

NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Manuel Chevalier <chevalier.manuel@gmail.com>'
    New submission

```

```
Build ID:	crestr_1.0.0.tar.gz-b57f7bfffecd44678fa9ca9e9946b952
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	32 minutes 22.6 seconds ago
Build time:	32 minutes 18.9 seconds

NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Manuel Chevalier <chevalier.manuel@gmail.com>’
    New submission

```

```
Build ID:	crestr_1.0.0.tar.gz-5c3d432d86854ec293f7652eda1f6d06
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	33 minutes 21.1 seconds ago
Build time:	33 minutes 18.4 seconds

NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Manuel Chevalier <chevalier.manuel@gmail.com>’
    New submission

```

### `check_on_linux()`

There were no ERRORs, WARNINGs or NOTEs.

```
crestr 1.0.0: OK

Build ID:	crestr_1.0.0.tar.gz-dc673de0252b4cba8194bb373b7c6b52
Platform:	Debian Linux, R-release, GCC
Submitted:	17 minutes 56.7 seconds ago
Build time:	17 minutes 51.3 seconds
```

### `check_on_windows()`

There were no ERRORs, WARNINGs or NOTEs.

```
crestr 1.0.0: OK

Build ID:	crestr_1.0.0.tar.gz-9919cfcd7a02431ca7e812efa3cfc5ae
Platform:	Windows Server 2008 R2 SP1, R-release, 32/64 bit
Submitted:	4 minutes 22.3 seconds ago
Build time:	4 minutes 14.1 seconds
```

### `check_on_fedora()`

There were no ERRORs, WARNINGs or NOTEs.

```
crestr 1.0.0: OK

Build ID:	crestr_1.0.0.tar.gz-6036f1ec9af147d89d5b6567672c8deb
Platform:	Fedora Linux, R-devel, GCC
Submitted:	32 minutes 7.9 seconds ago
Build time:	32 minutes 0.9 seconds
```

### `check_with_rrelease()`

There were no ERRORs, WARNINGs or NOTEs.

```
crestr 1.0.0: OK

Build ID:	crestr_1.0.0.tar.gz-08b855ec473f423683638ad89025890e
Platform:	Debian Linux, R-release, GCC
Submitted:	32 minutes 30.1 seconds ago
Build time:	32 minutes 22.2 seconds
```

# Downstream dependencies

There are no downstream dependencies for this package.
