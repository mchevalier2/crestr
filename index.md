



<!-- badges: start -->
<!-- badges: end -->

<br>

# **crestr** A R package to perform probabilistic palaeoclimate reconstructions from fossil bio-proxies

The goal of crestr is to produce probabilistic reconstructions of past  climate
change from fossil assemblage data. The approach is based on the estimation of
conditional responses of studies bio-proxy studied to climate parameters using
probability density functions (_PDFs_; see
[Chevalier *et al.* (2014)](http://www.doi.org/10.5194/cp-10-2081-2014) and
[Chevalier (2019)](http://www.doi.org/10.1016/j.gloplacha.2019.01.016)). The
theory underpinning this package is explained in section
[_A bit of theory_](https://mchevalier2.github.io/crestr/articles/theory.html)
and is illustrated with an application based on pseudo-data in section
[_Get Started_](https://mchevalier2.github.io/crestr/articles/get-started.html).
The different vignettes present different aspects of the structure of the
package and the data it contains, along with applications based on real data.

<br >

> **_NOTE:_**  If you notice any bug, or if you would like to see some specific
functions implemented, you can contact me at <chevalier.manuel@gmail.com>.


## Installation

You can install the development version from [GitHub](https://github.com/) with:


``` r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("mchevalier2/crestr")
```


## Example applications

<br >

<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/example-app.png" title="plot of chunk img1" alt="plot of chunk img1" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

_(A) density of presence records available in my calibration dataset upscaled at a 1° resolution. The diamonds represent the location of pollen records used to generate the reconstructions presented in B-D and the coloured boxes represent the extent of their respective calibration zones. (B) Annual precipitation reconstructions from Lake Van, Turkey ([Chevalier, 2019](http://www.doi.org/10.1016/j.gloplacha.2019.01.016)), (C) Mean annual temperature reconstruction from Laguna de Fùquene, Colombia (unpublished) and (D) Mean Annual temperature reconstruction from marine core MD96-2048 ([Chevalier et al., 2021](http://www.doi.org/10.1130/G47841.1))._


## CREST in the scientific litterature


<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crest-use-02.png" title="plot of chunk img3" alt="plot of chunk img3" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

- **2021**
    - Chevalier, M., Chase, B.M., Quick, L.J., Dupont, L.M. and Johnson, T.C., 2021, Temperature change in subtropical southeastern Africa during the past 790,000 yr. Geology, 49, pp. 71–75. [10.1016/j.palaeo.2021.110609](www.doi.org/10.1016/j.palaeo.2021.110609)
    - Hui, Z., Zhou, X., Chevalier, M., Wei, X., Pan, Y. and Chen, Y., 2021, Miocene East Asia summer monsoon precipitation variability and its possible driving forces. Palaeogeography, Palaeoclimatology, Palaeoecology, pp. 110609. [10.1130/G47841.1](www.doi.org/10.1130/G47841.1)
- **2020**
    - Yi, S., Jun, C.P., Jo, K. nam, Lee, H., Kim, M.S., Lee, S.D., Cao, X. and Lim, J., 2020, Asynchronous multi-decadal time-scale series of biotic and abiotic responses to precipitation during the last 1300 years. Scientific Reports, 10, pp. 1–10. [10.1038/s41598-020-74994-x](www.doi.org/10.1038/s41598-020-74994-x)
- **2019**
    - Chevalier, M., 2019, Enabling possibilities to quantify past climate from fossil assemblages at a global scale. Global and Planetary Change, 175, pp. 27–35. [10.1016/j.gloplacha.2019.01.016](www.doi.org/10.1016/j.gloplacha.2019.01.016)
- **2017**
    - Cordova, C.E., Scott, L., Chase, B.M. and Chevalier, M., 2017, Late Pleistocene-Holocene vegetation and climate change in the Middle Kalahari, Lake Ngami, Botswana. Quaternary Science Reviews, 171, pp. 199–215. [10.1016/j.quascirev.2017.06.036](www.doi.org/10.1016/j.quascirev.2017.06.036)
- **2016**
    - Lim, S., Chase, B.M., Chevalier, M. and Reimer, P.J., 2016, 50,000 years of climate in the Namib Desert, Pella, South Africa. Palaeogeography, Palaeoclimatology, Palaeoecology, 451, pp. 197–209. [10.1016/j.palaeo.2016.03.001](www.doi.org/10.1016/j.palaeo.2016.03.001)
    - Chevalier, M. and Chase, B.M., 2016, Determining the drivers of long-term aridity variability: a southern African case study. Journal of Quaternary Science, 31, pp. 143–151. [10.1002/jqs.2850](www.doi.org/10.1002/jqs.2850)
- **2015**
    - Chase, B.M., Lim, S., Chevalier, M., Boom, A., Carr, A.S., Meadows, M.E. and Reimer, P.J., 2015, Influence of tropical easterlies in the southwestern Cape of Africa during the Holocene. Quaternary Science Reviews, 107, pp. 138–148.[10.1016/j.quascirev.2014.10.011](www.doi.org/10.1016/j.quascirev.2014.10.011)
    - Chase, B.M., Boom, A., Carr, A.S., Carré, M., Chevalier, M., Meadows, M.E., Pedro, J.B., Stager, J.C. and Reimer, P.J., 2015, Evolving southwest African response to abrupt deglacial North Atlantic climate change events. Quaternary Science Reviews, 121, pp. 132–136. [10.1016/j.quascirev.2015.05.023](www.doi.org/10.1016/j.quascirev.2015.05.023)
    - Chevalier, M. and Chase, B.M., 2015, Southeast African records reveal a coherent shift from high- to low-latitude forcing mechanisms along the east African margin across last glacial–interglacial transition. Quaternary Science Reviews, 125, pp. 117–130. [10.1016/j.quascirev.2015.07.009](www.doi.org/10.1016/j.quascirev.2015.07.009)
- **2013**
    - Truc, L., Chevalier, M., Favier, C., Cheddadi, R., Meadows, M.E., Scott, L., Carr, A.S., Smith, G.F. and Chase, B.M., 2013, Quantification of climate change for the last 20,000 years from Wonderkrater, South Africa: implications for the long-term dynamics of the Intertropical Convergence Zone. Palaeogeography, Palaeoclimatology, Palaeoecology, 386, pp. 575–587. [10.1016/j.palaeo](www.doi.org/10.1016/j.palaeo)

<br >

<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crest-use-01.png" title="plot of chunk img2" alt="plot of chunk img2" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />


<img src="https://raw.githubusercontent.com/mchevalier2/crestr/master/webpage/crest-use-03.png" title="plot of chunk img4" alt="plot of chunk img4" width="100%" style="background:none; border:none; box-shadow:none;" style="display: block; margin: auto;" />

Last update: 23/08/2021

N.B.: This list is as exhaustive as possible but some studies may be missing. Contact me if you want your study to be added.

<br >

## References

 - Chevalier, M., Cheddadi, R., Chase, B.M., 2014. CREST (Climate REconstruction
   SofTware): a probability density function (PDF)-based quantitative climate
   reconstruction method. *Clim. Past* 10, 2081–2098.
   [10.5194/cp-10-2081-2014](http://www.doi.org/10.5194/cp-10-2081-2014)
 - Chevalier, M., 2019. Enabling possibilities to quantify past climate from
   fossil assemblages at a global scale. *Glob. Planet. Change* 175, 27–35.
   [10.1016/j.gloplacha.2019.01.016](http://www.doi.org/10.1016/j.gloplacha.2019.01.016)
 - Chevalier, M., Chase, B.M., Quick, L.J., Dupont, L.M. and Johnson, T.C., 2021.
   Temperature change in subtropical southeastern Africa during the past 790,000 yr.
   _Geology_ 49, 71–75. [10.1130/G47841.1](http://www.doi.org/10.1130/G47841.1)
