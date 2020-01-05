library(devtools)


install_github("karpatit/mechkar")

load("../data/BoxOffice_ff.RData")

library('mechkar')

exploreData(movies, y='revenue')

