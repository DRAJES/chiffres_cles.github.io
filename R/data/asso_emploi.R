library(readxl)

asso_emploi <- read_excel(path="C:/Users/plebre/Documents/projets R/github_chiffres_cles/data/engagement/asso_BFC.xlsx",
                   sheet=8,range="B5:E15")


save(asso_emploi,   file = "data/engagement/asso_emploi.RData")
