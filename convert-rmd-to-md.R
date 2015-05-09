rm(list=ls())

proj_path = "~/write/score-loan-applicants-book"
manu_path = file.path(proj_path, "manuscript")
setwd(manu_path)
rmd_path = file.path(manu_path, "Rmd")

opts_knit$set(out.format = "markdown")

file_in = file.path(rmd_path, "01-missing-value-treatment.Rmd")
knitr::knit(file_in)

