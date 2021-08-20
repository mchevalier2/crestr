
<!-- README.md is generated from README.Rmd. Please edit that file -->



<!-- badges: start -->
<!-- badges: end -->

<br>

# **crestr** A R package to perform probabilistic palaeoclimate reconstructions from fossil bioproxies

The goal of crestr is to produce probabilistic reconstructions of past  climate
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

> **_NOTE:_**  If you notice any bug, or if you would like to see some specific
functions implemented, feel free to contact me at <chevalier.manuel@gmail.com>.


## Example


<br >

<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/docs/articles/data-and-structure_files/figure-html/example-app.png" title="**Fig. 1**: (left) Climate variable to reconstruct (*e.g.* mean annual temperature). (Right) Four distinct plant taxa living in that region and having a preference for the darker climates (*e.g.* a preference for colder climates). These four species produce undistinguisable pollen grains and, therefore, define a 'pollen type'. This example is based on pseudo-data." alt="**Fig. 1**: (left) Climate variable to reconstruct (*e.g.* mean annual temperature). (Right) Four distinct plant taxa living in that region and having a preference for the darker climates (*e.g.* a preference for colder climates). These four species produce undistinguisable pollen grains and, therefore, define a 'pollen type'. This example is based on pseudo-data." width="80%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

<br >
