setwd('/Users/mchevali1/GitHub/Rpackages/crestr')
library(devtools)
load_all()

continentNames <- .getCountryNames()
realmNames <- .getRealmNames()

usethis::use_data(continentNames, realmNames, internal=TRUE, overwrite=TRUE)
