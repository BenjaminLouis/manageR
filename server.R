server <- function(input, output, session) {
  path <- callModule(wdLoad, "wd")
  devis <- callModule(loadingOptions, "files", path = path, filename = "Devis.csv")
  factures <- callModule(loadingOptions, "files", path = path, filename = "Factures.csv")
  clients <- callModule(loadingOptions, "files", path = path, filename = "Clients.csv")
  facturations <- callModule(loadingOptions, "files", path = path, filename = "Facturations.csv")
  prestations <- callModule(loadingOptions, "files", path = path, filename = "Prestations.csv")
  callModule(showData, "devis", data = devis)
  callModule(showData, "factures", data = factures)
  callModule(showData, "clients", data = clients)
  callModule(showData, "facturations", data = facturations)
  callModule(addData, "clients", data = clients, ncol = 4)
  callModule(addData, "facturations", data = clients, ncol = 4)
}