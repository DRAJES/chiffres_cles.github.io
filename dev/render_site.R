# renv::snapshot()     #renv sert à installer les packages lors du déployment github actions

#if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv") 

renv::restore() #on restaure les packages qu'on va utiliser pour construire (voir gh-pages.yml)

rm(list=ls())
gc()
rmarkdown::clean_site(encoding = 'UTF-8')
rmarkdown::render_site(encoding = 'UTF-8')


#fs::file_move(from = list.files(pattern = "\\.html$"), to = "docs/")

# bookdown::render_book(input = "index.Rmd",
#                       output_format = "html_document",
#                       encoding = 'UTF-8',
#                       output_dir = "docs/",
#                       config_file = "_site.yml")
