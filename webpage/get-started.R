
## The pseudo-data within `crestr`

library(crestr)
data(crest_ex)
data(crest_ex_pse)
data(crest_ex_selection)


## the first 6 samples
head(crest_ex)

## the structure of the data frame
str(crest_ex)

crest_ex_pse
crest_ex_selection


## Application: Reconstructions using pseudo-data

### Data extraction and calibration


reconstr <- crest.get_modern_data( df = crest_ex, # The fossil data
                              pse = crest_ex_pse, # The proxy-species equivalency table
                                    taxaType = 0, # The type of taxa is 0 for the example
                    climate = c("bio1", "bio12"), # The climate variables to reconstruct
               selectedTaxa = crest_ex_selection, # The taxa to use for each variable
                        dbname = "crest_example", # The database to extract the data
                                  verbose = FALSE # Print status messages
)


reconstr$inputs$selectedTaxa

reconstr

reconstr <- crest.calibrate( reconstr, # A crestObj produced at the previous stage
         climateSpaceWeighting = TRUE, # Correct the PDFs for the heteregenous
                                       # distribution of the modern climate space
                 bin_width = c(2, 50), # The size of bins used for the correction
     shape = c("normal", "lognormal"), # The shape of the species PDFs
                       verbose = FALSE # Print status messages
)

plot_climateSpace(reconstr)
plot_taxaCharacteristics(reconstr, taxanames='Taxon2', climate='bio1', h0=0.2)
plot_taxaCharacteristics(reconstr, taxanames='Taxon6', climate='bio1', h0=0.2)


### Reconstruction and interpretation

reconstr <- crest.reconstruct( reconstr, # A crestObj produced at the previous stage
                         verbose = FALSE # Print status messages
)

names(reconstr)

lapply(reconstr$reconstructions, names)

head(reconstr$reconstructions$bio1$optima)

str(reconstr$reconstructions$bio1$optima)

signif(reconstr$reconstructions$bio1$likelihood[1:6, 1:6], 3)

str(reconstr$reconstructions$bio1$likelihood)

plot(reconstr, climate = 'bio1')
plot(reconstr, climate = 'bio12', simplify=TRUE, uncertainties=c(0.4, 0.6, 0.8))

export(reconstr, loc=tempdir(), dataname='crest-test')
list.files(file.path(tempdir(), 'crest-test'))
