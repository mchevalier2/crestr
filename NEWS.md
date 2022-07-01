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
