# This is an exercise based on:
# Lapointe, F.-J. & P. Legendre. 1994. A classification of pure malt Scotch whiskies. Applied Statistics 43: 237-257. 

# dowloads from: http://adn.biol.umontreal.ca/~numericalecology/data/scotch.html

# There are 5 data sets: color, nose, body, palate, and finish. The binary (0-1) descriptors are in
# the same order as on p. 239 of the paper. In any case, they are all assembled (with identifiers) in
# an Excel data base, also included.
# 
# We also send you the list of geographic coordinates of the distilleries, given as decimal degrees:
# longitude WEST, followed by latitude NORTH. In Scotland, one degree north is about 1.87 times as
# long as one degree west. To be complete, We are also including a matrix of geographic distances among
# distilleries, already computed and written out as an ASCII file.
# 
# There are two whiskies in the classification from the Springbank distillery. One pertains to the Islay
# group, the other to the Western group.
# 
# Please let us know of the analysis you have performed, especially if you intend on publishing them. 


setwd("./scotch/")

temp = list.files(pattern="*.csv")

list2env(
  lapply(
    setNames(
      temp,
      make.names(gsub("*.csv$", "", temp))
    ), 
    function(x){read.csv(x, row.names = 1, header = T)}
  ),
  envir = .GlobalEnv
)

rm(temp)

require(vegan)
require(pipeR)

pipeline(
 rda(scotchData_pal),
 plot(type = "t")
)


pipeline(
  rda(scotchData_pal),
  plot(type = "t")
)

