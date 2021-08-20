---
title: "cran-comments"
author: "Manuel Chevalier"
date: "20/08/2021"
output: github_document
---

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

## R-HUB

Here, I have checked the package using five tests from the `rhub` package. The check_on_linux(), check_on_windows(), check_on_fedora() and check_on_rrelease() tests returned no ERRORs, WARNINGs or NOTEs. The check_for_cran() test consistently returned 2 NOTES:

    1. The first note is about this submission being a first submission.
    2. The second note is about some examples taking more than 5seconds to run. All the examples that issued a note are actually connecting to a remote database to extract data and process them. It is a process that is expected to take a bit a time. As such, these notes are completely expected.

### `check_for_cran()`


```{r eval=FALSE}
rhub::check_for_cran()
```

The result is:

```
Build ID:	crestr_1.0.0.tar.gz-e8e2775a10e34e1ebc41d97237b91cce
Platform:	Windows Server 2008 R2 SP1, R-devel, 32/64 bit
Submitted:	5 minutes 7.3 seconds ago
Build time:	5 minutes 5.3 seconds

NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Manuel Chevalier <chevalier.manuel@gmail.com>'
    New submission

* checking examples ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
                          user system elapsed
    crest.reconstruct     0.43   0.08    6.78
    crest                 1.39   0.09    7.90
    crest.get_modern_data 0.18   0.07    6.47
    crest.calibrate       0.19   0.02    6.36
```

```
Build ID:	crestr_1.0.0.tar.gz-e6ab0be557524025b3ae804f40a19abf
Platform:	Fedora Linux, R-devel, clang, gfortran
Submitted:	21 minutes 18.9 seconds ago
Build time:	21 minutes 17.3 seconds

NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Manuel Chevalier <chevalier.manuel@gmail.com>’
    New submission

* checking examples ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
                           user system elapsed
    crest                 1.053  0.118  60.335
    climate_from_xy       0.470  0.012   6.033
    crest.reconstruct     0.367  0.092  56.819
    crest.get_modern_data 0.258  0.062  56.472
    crest.calibrate       0.234  0.065  56.804
    getTaxonomy           0.038  0.018   9.024
    getDistribTaxa        0.036  0.017  11.357
    getTaxonID            0.039  0.002   8.483

```

```
Build ID:	crestr_1.0.0.tar.gz-f972dd37b0a6475c9319afa07b45d3dc
Platform:	Ubuntu Linux 20.04.1 LTS, R-release, GCC
Submitted:	20 minutes 21.3 seconds ago
Build time:	20 minutes 19.9 seconds

WARNINGS:

* checking re-building of vignette outputs ... WARNING
    Error(s) in re-building vignettes:
    --- re-building ‘get-started.Rmd’ using rmarkdown
    --- finished re-building ‘get-started.Rmd’

    --- re-building ‘limpopo.Rmd’ using rmarkdown
    --- finished re-building ‘limpopo.Rmd’

    --- re-building ‘theory.Rmd’ using rmarkdown
    Could not fetch https://raw.githubusercontent.com/mchevalier2/crestr/master/docs/articles/theory_files/figure-html/crest-01.png
    HttpExceptionRequest Request {
      host                 = "raw.githubusercontent.com"
      port                 = 443
      secure               = True
      requestHeaders       = []
      path                 = "/mchevalier2/crestr/master/docs/articles/theory_files/figure-html/crest-01.png"
      queryString          = ""
      method               = "GET"
      proxy                = Nothing
      rawBody              = False
      redirectCount        = 10
      responseTimeout      = ResponseTimeoutDefault
      requestVersion       = HTTP/1.1
    }
     (ConnectionFailure Network.BSD.getProtocolByName: does not exist (no such protocol name: tcp))
    Error: processing vignette 'theory.Rmd' failed with diagnostics:
    pandoc document conversion failed with error 61
    --- failed re-building ‘theory.Rmd’

    SUMMARY: processing the following file failed:
      ‘theory.Rmd’

    Error: Vignette re-building failed.
    Execution halted

NOTES:

* checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Manuel Chevalier <chevalier.manuel@gmail.com>’
    New submission

* checking examples ... NOTE
    Examples with CPU (user + system) or elapsed time > 5s
                           user system elapsed
    crest                 1.005  0.156  60.803
    climate_from_xy       0.461  0.015   6.140
    crest.reconstruct     0.366  0.090  57.109
    crest.get_modern_data 0.211  0.096  57.397
    crest.calibrate       0.210  0.083  56.846
    getDistribTaxa        0.036  0.021  11.276
    getTaxonomy           0.038  0.016   8.930
    getTaxonID            0.037  0.006   8.643
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

Build ID:	crestr_1.0.0.tar.gz-466d7d879a604a0e8a5f4183a2aadcc7
Platform:	Windows Server 2008 R2 SP1, R-release, 32/64 bit
Submitted:	5 minutes 20.9 seconds ago
Build time:	5 minutes 9.5 seconds
```

### `check_on_fedora()`

There were no ERRORs, WARNINGs or NOTEs.

```
crestr 1.0.0: OK

Build ID:	crestr_1.0.0.tar.gz-6ddcffb5d45449c89f57276cc15f6592
Platform:	Fedora Linux, R-devel, GCC
Submitted:	18 minutes 19.9 seconds ago
Build time:	18 minutes 17 seconds
```

### `check_on_rrelease()`

There were no ERRORs, WARNINGs or NOTEs.

```
crestr 1.0.0: OK

Build ID:	crestr_1.0.0.tar.gz-d067d0a530ac401eba1444792672b17c
Platform:	Debian Linux, R-release, GCC
Submitted:	18 minutes 52.5 seconds ago
Build time:	18 minutes 41.6 seconds
```

# Downstream dependencies

There are no downstream dependencies for this package.
