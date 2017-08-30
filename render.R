setwd("/git/tb_vaccination")
rmarkdown::render("tb_vaccination.Rmd",output_format = "all", output_dir="docs")
file.rename("docs/tb_vaccination.nb.html","docs/index.html")