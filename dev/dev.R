library(devtools)

document()
check()
build()

# initial setup ----
# use_data_table()
# use_build_ignore('dev')
# use_package('afscdata')
# use_mit_license()
# use_pipe()
# use_package('data.table')
# use_package('tidytable')
# use_package('terra')
# use_package('crayon')
# use_package('ggplot2')
# use_package('scico')
# use_package('tickr')
# use_package('afscassess')
# use_package('scales')
# use_package('dbplyr')
# use_package('here')
# use_package('dplyr')
# use_package('stats')
# use_package('vroom')
# use_r('query_data')
# use_r('restrat')
# use_r('get_index')
# use_r('compare_index')
# use_r('plot_restrat')

# example run ----
library(goaRestrat)
# tier 3 species codes
species_t3 = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
setwd(here::here('dev'))

data <- query_data(species=species_t3)
# reclassify haul station locations ----
new_haul <- restrat(data, run_terra=TRUE)


# compute abundance indices (biom & numbers) ----
index <- get_index(data=data, new_haul=new_haul)


## check that computed indices matches with gap produced indices (within 0.1% on average) ---
compare_index(data, index)


# plot comparison between restratified and og indices ----

# example for pacific cod
species = 21720
stock_name = "Pcod"

plot_restrat(index, species, stock_name)

# example for complex (rebs)
species = c(30050, 30051, 30052)
stock_name = "REBS"

plot_restrat(index, species, stock_name)
