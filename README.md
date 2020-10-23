
<!-- README.md is generated from README.Rmd. Please edit that file -->

# crestr

<!-- badges: start -->
<!-- badges: end -->

The goal of crestr is to produce probabilistic reconstructions of past
climate change from fossil assemblage data.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("mchevalier2/crestr")

## Example

This is a basic example which shows you how to run crest with randomly
generated data:

    library(crestr)
    ## loading example data
    data(crest_ex)
    data(crest_ex_pse)
    data(crest_ex_selection)

Let’s first have a look at the data. We have 20 fossil samples from
which 7 taxa have been identified.

    ## the first 6 samples
    head(crest_ex)
    #>          Age Taxon1 Taxon2 Taxon3 Taxon4 Taxon5 Taxon6 Taxon7
    #> Sample_1   1      0      0     45      1     22     32      1
    #> Sample_2   2      0      0     50      0     23     27      0
    #> Sample_3   3      0      0     49      0     25     26      0
    #> Sample_4   4      0      0     37      0     27     36      0
    #> Sample_5   5      0      3     36      3     18     40      0
    #> Sample_6   6      2      2     25      0     21     50      0
    ## the structure of the data frame
    str(crest_ex)
    #> 'data.frame':    20 obs. of  8 variables:
    #>  $ Age   : int  1 2 3 4 5 6 7 8 9 10 ...
    #>  $ Taxon1: int  0 0 0 0 0 2 3 5 10 15 ...
    #>  $ Taxon2: int  0 0 0 0 3 2 5 5 12 8 ...
    #>  $ Taxon3: int  45 50 49 37 36 25 18 17 10 12 ...
    #>  $ Taxon4: int  1 0 0 0 3 0 0 6 15 14 ...
    #>  $ Taxon5: int  22 23 25 27 18 21 21 20 16 13 ...
    #>  $ Taxon6: int  32 27 26 36 40 50 53 47 37 38 ...
    #>  $ Taxon7: int  1 0 0 0 0 0 0 0 0 0 ...

For ech reconstruction, a proxy-species equivalency (‘pse’) table must
be provided. Here, with the 7 fake taxa, it looks like:

    crest_ex_pse
    #>   Level      Family    Genus Species ProxyName
    #> 1     3 Randomaceae Randomus  Taxon1    Taxon1
    #> 2     3 Randomaceae Randomus  Taxon2    Taxon2
    #> 3     3 Randomaceae Randomus  Taxon3    Taxon3
    #> 4     3 Randomaceae Randomus  Taxon4    Taxon4
    #> 5     3 Randomaceae Randomus  Taxon5    Taxon5
    #> 6     3 Randomaceae Randomus  Taxon6    Taxon6
    #> 7     3 Randomaceae Randomus  Taxon7    Taxon7

Finally, one can specify which taxa should be used to reconstruct each
variable:

    crest_ex_selection
    #>        bio1 bio12
    #> Taxon1    1     1
    #> Taxon2    1     0
    #> Taxon3    1     0
    #> Taxon4    0     1
    #> Taxon5    0     1
    #> Taxon6    0     0
    #> Taxon7    1     1

To illustrate the process, we will reconstruct bio1 (mean annual
temperature) and bio12 (annual precipitation) from these fake data. The
description of the different parameters is available in the first
*vignette*.

    recons <- crest(
       df = crest_ex, pse = crest_ex_pse, taxaType = 0,
       climate = c("bio1", "bio12"), bin_width = c(2, 20),
       shape = c("normal", "lognormal"),
       selectedTaxa = crest_ex_selection, dbname = "crest_example"
    )
    #> [1] "make a quality test for taxaType."
    #> No match for taxon  Randomaceae, Randomus, Taxon7, Taxon7 
    #> Extracting data from the online database.
    #>        |                                                |                                        |   0%  |                                                |======                                  |  14%  |                                                |===========                             |  29%  |                                                |=================                       |  43%  |                                                |=======================                 |  57%  |                                                |=============================           |  71%  |                                                |==================================      |  86%  |                                                |========================================| 100%

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub!
