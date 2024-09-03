##
# Retrieved from:
# https://www4.stat.ncsu.edu/~stefansk/NSF_Supported/Hidden_Images/UNCG_Helen_Barton_Lecture_Nov_2013/pumpkin_1_data_yx1x6.txt
raw <- read.csv("data-raw/pumpkin_1_data_yx1x6.txt")

colnames(raw) <- c("y", paste0("x", 1:6))

jackolantern_surreal_data <- raw

usethis::use_data(jackolantern_surreal_data, overwrite = TRUE)
