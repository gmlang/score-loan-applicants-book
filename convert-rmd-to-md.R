rm(list=ls())

library(knitr)
opts_knit$set(out.format = "markdown")

proj_path = "~/write/score-loan-applicants-book"
manu_path = file.path(proj_path, "manuscript")
setwd(manu_path)
rmd_path = file.path(manu_path, "Rmd")

file_in = file.path(rmd_path, "01-set-up.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "02-01-clean-data.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "02-02-descriptive-analysis.Rmd")
knitr::knit(file_in)
