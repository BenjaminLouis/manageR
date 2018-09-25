library(here)
library(backports) #bug : sinon fonction strrep() n'est pas trouvé --> faire une issue (pas de soucis sous windows)
library(sassr)

doc <- "Facture"
ndoc <- "Test-0001"
write(x = paste0("$columns: 12; \n$doc: \"", doc, "\"; \n$ndoc: \"", ndoc, "\";"), file = "_variables.scss")
compile_sass(file = "template_style.scss", output = "template_style.css")



