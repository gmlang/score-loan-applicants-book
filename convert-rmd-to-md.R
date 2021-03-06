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

file_in = file.path(rmd_path, "02-03-WoE-n-IV.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "02-04-simple-logit.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "02-05-preselect-predictors.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "02-06-correlations.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "03-01-model-selection-using-bestsubsets.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "03-02-backtesting.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "03-03-performance-curves.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "03-04-cross-validation.Rmd")
knitr::knit(file_in)

file_in = file.path(rmd_path, "03-05-final-model.Rmd")
knitr::knit(file_in)
