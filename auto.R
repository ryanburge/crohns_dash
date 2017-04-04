setwd("D:/crohns_dash")
library(rmarkdown)
rmarkdown::render("dashboard.Rmd", output_dir = "C:/Users/Ryan Burge/Dropbox/Apps/BoxHop/crohns", output_file = "index.html")