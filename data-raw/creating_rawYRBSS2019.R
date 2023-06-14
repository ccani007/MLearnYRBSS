# Importing the original 2019 YRBSS data
# Catalina Canizares
# 05-11-2023


# This data comes from the tidyYRBS package in my 
# github repo: https://github.com/ccani007/tidyYRBS.git

load("inst/extData/raw_yrbs_2019.rda")

rawYRBSS2019 <- raw_yrbs_2019

usethis::use_data(rawYRBSS2019 , overwrite = TRUE)
