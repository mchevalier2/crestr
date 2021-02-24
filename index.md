
<!-- README.md is generated from README.Rmd. Please edit that file -->



<!-- badges: start -->
<!-- badges: end -->

<br>

# **crestr** A R package to perform probabilistic palaeoclimate reconstructions from fossil bioproxies

The goal of crestr is to produce probabilistic reconstructions of past climate
change from fossil assemblage data. The approach is based on the estimation of
conditional responses of studies bioproxy studied to climate paramters. These
responses take the form of probability density functions (*pdfs*). The details
of the method have been described in [Chevalier *et al.* (2014)](http://www.doi.org/10.5194/cp-10-2081-2014)
and the calibrated data presented in [Chevalier (2019)](http://www.doi.org/10.1016/j.gloplacha.2019.01.016).

## Installation

You can install the development version from [GitHub](https://github.com/) with:


``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("mchevalier2/crestr")
```

> **_NOTE:_**  This package is still in an **early phase of development**. The
documentation is sparse and the functionalities limited. If you notice any bug,
or if you would like to see some specific functions implemented, feel free to contact
me at <chevalier.manuel@gmail.com>. I will try to limit as much as possible
modifying existing functions and/or function parameters, but at the moment it
cannot be excluded. My apologies for this.

## Example

This is a basic example which shows you how to run crest with randomly generated
data:


```r
library(crestr)
## loading example data
data(crest_ex)
data(crest_ex_pse)
data(crest_ex_selection)
```

Let's first have a look at the data. We have 20 fossil samples from which 7 taxa
have been identified. The data are already expressed in percentages.


```r
## the first 6 samples
head(crest_ex)
#>          Age Taxon1 Taxon2 Taxon3 Taxon4 Taxon5 Taxon6 Taxon7
#> Sample_1   1      0      0     45      1     22     32      1
#> Sample_2   2      0      0     50      0     23     27      0
#> Sample_3   3      0      0     49      0     25     26      0
#> Sample_4   4      0      0     37      0     27     36      0
#> Sample_5   5      0      3     36      3     18     40      0
#> Sample_6   6      2      2     25      0     21     50      0
```


```r
## the structure of the data frame
str(crest_ex)
#> 'data.frame':	20 obs. of  8 variables:
#>  $ Age   : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ Taxon1: int  0 0 0 0 0 2 3 5 10 15 ...
#>  $ Taxon2: int  0 0 0 0 3 2 5 5 12 8 ...
#>  $ Taxon3: int  45 50 49 37 36 25 18 17 10 12 ...
#>  $ Taxon4: int  1 0 0 0 3 0 0 6 15 14 ...
#>  $ Taxon5: int  22 23 25 27 18 21 21 20 16 13 ...
#>  $ Taxon6: int  32 27 26 36 40 50 53 47 37 38 ...
#>  $ Taxon7: int  1 0 0 0 0 0 0 0 0 0 ...
```
