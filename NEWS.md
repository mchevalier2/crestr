# crestr 1.4.3
* MAJOR
    * Identified and fixed a problem with `crest.set_modern_data()` that appeared with v1.4.0. The modern data were not distributed to the correct species when weight was set to TRUE (no problem with weight=FALSE). The problem is fixed.
* MINOR
    * The column for weights in the `distributions` table can be spelled 'weight' or 'Weight'.
    * The labelling bug in `plot_violinPDFs()` associated with private calibration dataset is fixed.

# crestr 1.4.2
* Fixing web address of gbif4crest_03 in `dbDownload()``

# crestr 1.4.1
* Adding CITATION.cff

# crestr 1.4.0

* Improving compatibility with new calibration database
    * `getResol()`
    * `f_locid()`
    * `identifyDatabase()`
    * Better factorization of the code
    * Adding new references to `cite_crest()`
* Fixing issues with `crest.set_modern_data()`
    * Ok to pass input data in tibble format.
    * Logging of taxa assignment is fixed.
    * Including minGridCell in the algorithm.
    * Adding a global warning if mingridcell is <15, and an error is <2.
    * The plots that require variable names function properly.
* Minor issues:
    * Cleaned documentation



# crestr 1.3.0

* The dependencies to `rgdal`, `sp` and `raster` were replaced to a single dependency to `terra` as these packages are being retired.
* New functions and features:
    * `crest.simplify()` transforms a crestObj into a simple data frame with only the main reconstructed values.
    * `dbSubset()` extracts a subset of the calibration dataset and makes data extraction much faster.
    * `dbDownload()` allows downloading the calibration dataset on your machine.
    * The example dataset is now available offline.
    * `loo()` can be run on a subset of the available variables to make the process faster.
    * `getTaxonomy()` can be used with a `crestObj()` to refine the search to a specific study area.
    * Including a new parameter to `crest.calibrate()`: `climateSpaceWeighting.type`. This parameter is useful to reduce the edge effects to the climate space calibration, as the range of weights could easily cover several ranges of amplitude. This can be toned down using 'sqrt' or 'log', instead of the default 'linear'.
    * `crest.get_modern_data()` now extract geopolitcal and biogeophical information from the _gbif4crest_ database.
* Plotting:
    * The results can be plotted as anomalies.
    * Enabling to select samples by age in `plot.speciesCharacteristics()`
    * The correct filename extensions (.pdf or .png) is automatically in all plotting functions.
    * Many visual improvements when exporting the plots to pdf or png.
* Major issues:
    * Fixing a major stability issue regarding the connection test (does not impact any result derived from the package)
    * Reliance on an active internet connection has been significantly reduced.
    * Portability of the RData output file was increased.
* Minor issues:
    * If the input `df` contained NA, `crest.get_modern_data()` was crashing. Default value now set to 0.
    * Updating vignettes (reducing the amount of code actually ran to create the vignette).
    * Database handlers can better handle the reduced list of parameters available in the calibration databse
    * The north and east labels in `export()` were inverted.
    * The `ylim` parameter in `plot(simplify=TRUE)` did not allow to select a y-range larger than the range of the uncertaintieS.
    * Including more quality tests for the functions parameters (e.g. climate, taxa, taxanames, samples, uncertainties, bin_width).


# crestr 1.2.1

* Getting rid of one warning in `export()`
* Replacing epsg:4326 by  `+proj=longlat +datum=WGS84 +no_defs` to potentially avoid bugs when plotting maps (problem ultimately related to latest version of gdal and proj)
* Problem of non-recorded taxa in selectedtaxa caused the final plots to crash. they are now excluded and set a value of -2.
* Adding a CRAN-compatible way of dealing with failed online connections (includes checking if is.crestObj(x) in every function).
* Minor corrections
    * Removing unnecessary prints
    * Improved documentation


# crestr 1.2.0

* Removing some unnecessary debugging prints
* Adapting the crestr reference
* Adding a check on the column names of PSE
* Fastening the LOO function
* Adding a sorting parameter to the `LOO()` function
* Adding the `pdf_ranges()` function.
    * Also included as an invisible output of the `plot_violinPDFs()`
* Adding the `plot.scatterPDFs()` graphical output.
* Adding `taxonComposition()` to get the size distribution of the composing species.
* Minor bugs fixed
    * If no distributions are extracted, the calibration could be used.
    * `combinedPDFs()` could bug if only one variable
    * Filters by elevation added to `getClimateSpace()`
    * print(ntaxa) was 1 off
    * Minor naming issues with `export()`
    * `plot_climateSpace()` and `plot_taxaCharacteristics()` were issuing a warning when no climate values are available and add_modern is TRUE (coordinates are available but no climate)


# crestr 1.1.0

* crestr is now compatible with the SQLite3 gbif4crest database. It is therefore usable offline.
    * Access: https://figshare.com/articles/dataset/GBIF_for_CREST_database/6743207


# crestr 1.0.3

* Fixing minor bugs, including:
    * If the `climate` field didn't match the column names of `selectedTaxa`, some functions were crashing (at least `plot_climateSpace`) because no distribution data were extracted.
    * Checks to ensure the column names of climate_space and distributions are correct for `crest.set_modern_data()`
    * Check to ensure the `selectedTaxa` has default values for all selected variables.



# crestr 1.0.2

* Replaced class(dat) == 'RasterLayer' by inherits(dat, 'RasterLayer')

* Improved documentation


# crestr 1.0.1

* Minor bugs fixed on the plots
    * Title of the LOO plot adapted from 'variable name [unit]' to 'Leave-one-out anomalies for\nvariable name [unit]'
    * Title of stratigraphic diagrams can spread across many lines now
    * The thickness of the lines on the samplePDFs plot has been revised to show a greater range.
    * Labels added to the y-axes on the histograms of the climate space plot.
    * Labels added to the y-axis of the histograms and pdf plots of the taxaCharacteristics figure.
    * Adding caption on LOO diagram (+ and - anomalies)
    * Fixing title problem on plot_diagram()
    * Adding a colouring option for hiatus(es) on plot.crestObj()
    * Fixed problem of opening empty plotting windows when exporting the figures to files
    * Changing default value for bar_width in plot_loo() and plot_diagram()


* Replacing the parameter fullPosterior by fullUncertainties in export().

* Typos in the documentation have been fixed.

* The text of the vignettes was updated to reflect changes in the manuscript. Nothing really significant.

* Fixing bug related to ai.sqrt in crest.get_modern_data(). Only the climate space data were transformed.

* Excluding all the distribution grid cells without any climate data.




# crestr 1.0.0

* Release of the first stable version of `crestr`


# crestr 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
