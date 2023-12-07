



<!-- badges: start -->
<!-- badges: end -->

<br>

# **crestr** An R package to perform probabilistic palaeoclimate reconstructions from palaeoecological datasets

The goal of `crestr` is to produce probabilistic reconstructions of past climate change from fossil assemblage data [(Chevalier, 2022)](https://cp.copernicus.org/articles/18/821/2022/). The approach is based on the estimation of responses of studies bio-proxy studied to climate parameters using probability density functions (_PDFs_; see [Chevalier *et al.* (2014)](https://www.doi.org/10.5194/cp-10-2081-2014) and [Chevalier (2019)](https://www.doi.org/10.1016/j.gloplacha.2019.01.016)). The theory underpinning this package is explained in section [_A bit of theory_](https://www.manuelchevalier.com/crestr/articles/theory.html) and is illustrated with an application based on pseudo-data in section [_Get Started_](https://www.manuelchevalier.com/crestr/articles/get-started.html). The different vignettes present different aspects of the structure of the package and the data it contains, along with applications based on real data.

<br >

> **_NOTE:_**  If you notice any bug, or if you would like to see some specific
functions implemented, you can contact me at <chevalier.manuel@gmail.com>.


## Installation

The package is currently unavailable from CRAN. However, you can install the latest development version (1.3.0) from [GitHub](https://github.com/) with:

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("mchevalier2/crestr")
```

Or the latest stable version archived by CRAN. If you use that version, keep in mind that not all functionalities will be available (See News page).

``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_version("crestr", version = "1.2.0", repos = "https://cran.us.r-project.org")
```


## _crestr_ in a glimpse

You can now quickly visualise how to run a reconstruction with the _crestr_ cheat sheet. This document summarises the main functionalities of the package and illustrates the main key step you will have to follow to reconsturct environmental parameters from your data. [download here](https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crestr_cheatsheet_v1.3.0.pdf)

<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crestr_cheatsheet_thumbnail.png" width="60%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

## Example applications

<br >

<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/example-app.png" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

_(A) density of presence records available in my calibration dataset upscaled at a 1° resolution. The diamonds represent the location of the pollen records used to generate the reconstructions presented in B-D, and the coloured boxes represent the extent of their respective calibration zones. (B) Annual precipitation reconstructions from Lake Van, Turkey ([Chevalier, 2019](https://www.doi.org/10.1016/j.gloplacha.2019.01.016)), (C) Mean annual temperature reconstruction from Laguna Fùquene, Colombia (unpublished) and (D) Mean Annual temperature reconstruction from marine core MD96-2048 ([Chevalier et al., 2021](https://www.doi.org/10.1130/G47841.1))._


## CREST in the scientific literature


<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crest-use-02.png" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

- **2023**
    - Hui, Z., Wei, X., Xue, Z., Zhao, X., Chevalier, M., Lu, X., Zhang, J., Peng, T., Chen, Y., Chen, P., 2023,Middle Miocene Evolution of East Asian Summer Monsoon Precipitation in the Northeast Part of the Tibetan Plateau Based on a Quantitative Analysis of Palynological Records. Palaeogeography, Palaeoclimatology, Palaeoecology, pp 111808. [10.1016/j.palaeo.2023.111808](https://doi.org/10.1016/j.palaeo.2023.111808).
    - Hui, Z., Liu, J., Chevalier, M., Wei, X., Chen, P., Zhan, J., Peng, T., Zhou, X., 2023, Multiple forcing on Late Miocene East Asian Summer Monsoon Precipitation Variability in NE Tibetan Plateau. CATENA 221, 106752. [10.1016/j.catena.2022.106752](https://doi.org/10.1016/j.catena.2022.106752).
- **2022**
    - Gibson, M.E., McCoy, J., O’Keefe, J.M.K., Nuñez Otaño, N.B., Warny, S., Pound, M.J., 2022, Reconstructing Terrestrial Paleoclimates: A Comparison of the Co‐Existence Approach, Bayesian and Probability Reconstruction Techniques Using the UK Neogene. Paleoceanog. and Paleoclimatol. 37, e2021PA004358. [10.1029/2021PA004358](https://doi.org/10.1029/2021PA004358)
- **2021**
    - Chevalier, M., Chase, B.M., Quick, L.J., Dupont, L.M. and Johnson, T.C., 2021, Temperature change in subtropical southeastern Africa during the past 790,000 yr. Geology, 49, pp. 71–75. [10.1016/j.palaeo.2021.110609](https://www.doi.org/10.1016/j.palaeo.2021.110609)
    - Hui, Z., Zhou, X., Chevalier, M., Wei, X., Pan, Y. and Chen, Y., 2021, Miocene East Asia summer monsoon precipitation variability and its possible driving forces. Palaeogeography, Palaeoclimatology, Palaeoecology, pp. 110609. [10.1130/G47841.1](https://www.doi.org/10.1130/G47841.1)
    - Quick, L.J., Chase, B.M., Carr, A.S., Chevalier, M., Grobler, B.A. and Meadows, M.E., 2021, A 25,000 year record of climate and vegetation change from the southwestern Cape coast, South Africa. Quaternary Research, pp. 1–18. [10.1017/qua.2021.31](https://www.doi.org/10.1017/qua.2021.31)
    - Romero, I. C., Nuñez Otaño, N. B., Gibson, M. E., Spears, T. M., Fairchild, C. J., Tarlton, L., Jones, S., Belkin, H. E., Warny, S., Pound, M. J. and O’Keefe, J. M. K., 2021, First Record of Fungal Diversity in the Tropical and Warm-Temperate Middle Miocene Climate Optimum Forests of Eurasia, Frontiers in Forests and Global Change, 1–18. [10.3389/ffgc.2021.768405](https://www.doi.org/10.3389/ffgc.2021.768405)
- **2020**
    - Yi, S., Jun, C.P., Jo, K. nam, Lee, H., Kim, M.S., Lee, S.D., Cao, X. and Lim, J., 2020, Asynchronous multi-decadal time-scale series of biotic and abiotic responses to precipitation during the last 1300 years. Scientific Reports, 10, pp. 1–10. [10.1038/s41598-020-74994-x](https://www.doi.org/10.1038/s41598-020-74994-x)
- **2019**
    - Chevalier, M., 2019, Enabling possibilities to quantify past climate from fossil assemblages at a global scale. Global and Planetary Change, 175, pp. 27–35. [10.1016/j.gloplacha.2019.01.016](https://www.doi.org/10.1016/j.gloplacha.2019.01.016)
- **2017**
    - Cordova, C.E., Scott, L., Chase, B.M. and Chevalier, M., 2017, Late Pleistocene-Holocene vegetation and climate change in the Middle Kalahari, Lake Ngami, Botswana. Quaternary Science Reviews, 171, pp. 199–215. [10.1016/j.quascirev.2017.06.036](https://www.doi.org/10.1016/j.quascirev.2017.06.036)
- **2016**
    - Lim, S., Chase, B.M., Chevalier, M. and Reimer, P.J., 2016, 50,000 years of climate in the Namib Desert, Pella, South Africa. Palaeogeography, Palaeoclimatology, Palaeoecology, 451, pp. 197–209. [10.1016/j.palaeo.2016.03.001](https://www.doi.org/10.1016/j.palaeo.2016.03.001)
    - Chevalier, M. and Chase, B.M., 2016, Determining the drivers of long-term aridity variability: a southern African case study. Journal of Quaternary Science, 31, pp. 143–151. [10.1002/jqs.2850](https://www.doi.org/10.1002/jqs.2850)
- **2015**
    - Chase, B.M., Lim, S., Chevalier, M., Boom, A., Carr, A.S., Meadows, M.E. and Reimer, P.J., 2015, Influence of tropical easterlies in the southwestern Cape of Africa during the Holocene. Quaternary Science Reviews, 107, pp. 138–148.[10.1016/j.quascirev.2014.10.011](https://www.doi.org/10.1016/j.quascirev.2014.10.011)
    - Chase, B.M., Boom, A., Carr, A.S., Carré, M., Chevalier, M., Meadows, M.E., Pedro, J.B., Stager, J.C. and Reimer, P.J., 2015, Evolving southwest African response to abrupt deglacial North Atlantic climate change events. Quaternary Science Reviews, 121, pp. 132–136. [10.1016/j.quascirev.2015.05.023](https://www.doi.org/10.1016/j.quascirev.2015.05.023)
    - Chevalier, M. and Chase, B.M., 2015, Southeast African records reveal a coherent shift from high- to low-latitude forcing mechanisms along the east African margin across last glacial–interglacial transition. Quaternary Science Reviews, 125, pp. 117–130. [10.1016/j.quascirev.2015.07.009](https://www.doi.org/10.1016/j.quascirev.2015.07.009)
- **2013**
    - Truc, L., Chevalier, M., Favier, C., Cheddadi, R., Meadows, M.E., Scott, L., Carr, A.S., Smith, G.F. and Chase, B.M., 2013, Quantification of climate change for the last 20,000 years from Wonderkrater, South Africa: implications for the long-term dynamics of the Intertropical Convergence Zone. Palaeogeography, Palaeoclimatology, Palaeoecology, 386, pp. 575–587. [10.1016/j.palaeo](https://www.doi.org/10.1016/j.palaeo)

<br >

<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crest-use-01.png" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />


<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crest-use-03.png" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

Last update: 03/01/2023

N.B.: This list is as exhaustive as possible, but some studies may be missing. Contact me if you want your study to be added.

<br >

## References

 - Chevalier, M., Cheddadi, R., Chase, B.M., 2014. CREST (Climate REconstruction
   SofTware): a probability density function (PDF)-based quantitative climate
   reconstruction method. *Clim. Past* 10, 2081–2098.
   [10.5194/cp-10-2081-2014](https://www.doi.org/10.5194/cp-10-2081-2014)
 - Chevalier, M., 2019. Enabling possibilities to quantify past climate from
   fossil assemblages at a global scale. *Glob. Planet. Change* 175, 27–35.
   [10.1016/j.gloplacha.2019.01.016](https://www.doi.org/10.1016/j.gloplacha.2019.01.016)
 - Chevalier, M., Chase, B.M., Quick, L.J., Dupont, L.M. and Johnson, T.C., 2021.
   Temperature change in subtropical southeastern Africa during the past 790,000 yr.
   _Geology_ 49, 71–75. [10.1130/G47841.1](https://www.doi.org/10.1130/G47841.1)
 - Chevalier, M., 2022. _crestr_ an R package to perform probabilistic climate
   reconstructions from palaeoecological datasets. Clim. Past
   [doi:10.5194/cp-18-821-2022](https://www.doi.org/10.5194/cp-18-821-2022)
