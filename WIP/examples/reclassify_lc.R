library(sprawl)
library(magrittr)

in_file <- "D:/tmp_sprawl/rast/LC/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif"

in_rast <- read_rast(in_file)

class_matrix <- tibble::tribble(
  ~start, ~end, ~new,
-Inf,    10,   NA, #NA
  10,   40,   1,  #Cropland
  40,   50,   2,  # Mixed Crop/NatVeg
  50,   70,   3,  # Broadleaved Forest
  70,   81,   4,  # Needleleaved Forest
  81,   100,   5,  # Mixed Forest
  100,  190,   6,  # Shrubland/Sparse Veg
  190,  200,   7,   #Urban Areas
  200,  210,   8,  #Bare Areas
  210,  220,   9,  # Water
  220,  Inf,   10  # Snow/Ice
  )

class_names <- c("Cropland", #1
                 "Mixed Crop/NatVeg", #2
                 "Broadleaved Forest", #3
                 "Needleleaved Forest", #4
                 "Mixed Forest", #5
                 "Shrubland/Sparse Veg", #6
                 "Urban Areas", #7
                 "Bare Areas", #8
                 "Water", #9
                 "Snow/Ice" #9
                 )

in_rast_rec <- in_rast %>%
  categorize_rast(class_matrix = class_matrix,
    out_file = "D:/tmp_sprawl/rast/LC/LC_ESA_reclass.tif")

