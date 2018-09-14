server <- function(input, output, session) {
  
  # Home
  # ----
  path <- callModule(wdLoad, "wd")
  initestimates <- callModule(loadingOptions, "files", path = path, filename = "Estimates.csv")
  initbills <- callModule(loadingOptions, "files", path = path, filename = "Bills.csv")
  initclients <- callModule(loadingOptions, "files", path = path, filename = "Clients.csv")
  initaddresses <- callModule(loadingOptions, "files", path = path, filename = "Addresses.csv")
  initservices <- callModule(loadingOptions, "files", path = path, filename = "Services.csv")
  
  # Cost Estimates
  # --------------
  callModule(showData, "estimates", data = initestimates)
  
  # Bills
  # -----
  callModule(showData, "bills", data = initbills)
  
  # Clients
  # -------
  clients <- callModule(editableDT, "clients", data = initclients, path = path, filename = "Clients.csv")#, ncol = 4)
  
  # Billing addresses
  # -----------------
  addresses <- callModule(editableDT, "addresses", data = initaddresses, path = path, filename = "Addresses.csv")#, ncol = 4)
}