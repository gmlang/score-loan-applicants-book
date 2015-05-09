rm(list=ls())

proj_path = "~/write/score-loan-applicants-book"
manu_path = file.path(proj_path, "manuscript")
setwd(manu_path)
rmd_path = file.path(manu_path, "Rmd")

file_in = file.path(rmd_path, "01-missing-value-treatment.Rmd")
file_out = file.path(manu_path, "01-missing-value-treatment.md")

# opts_knit$set(unnamed.chunk.label = "fig", out.format = "markdown")

opts_knit$set(out.format = "markdown")
knitr::knit(file_in, file_out)

