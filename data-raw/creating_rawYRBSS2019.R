# Importing the original 2019 YRBSS data
# Catalina Canizares
# 05-11-2023


# This data comes from the tidyYRBS package in my 
# github repo: https://github.com/ccani007/tidyYRBS.git

rawYRBSS2019 <- load("inst/extData/raw_yrbs_2019.rda")

usethis::use_data(rawYRBSS2019 , overwrite = TRUE)
