library(here)
library(sassr)

doc <- "Facture"
ndoc <- "Test-0001"
write(x = paste0("$columns: 12; \n$doc: \"", doc, "\"; \n$ndoc: \"", ndoc, "\";"), file = "_variables.scss")
compile_sass(file = "template_style.scss", output = "template_style.css")



