server <- function(input, output, session) {
  
  # Home
  # ----
  path <- callModule(wdLoad, "wd")
  initestimates <- callModule(loadingOptions, "files", path = path, filename = "Estimates.csv", coltypes = cols(.default = col_character(), Date = col_date(format = "%d-%m-%Y")))
  initbills <- callModule(loadingOptions, "files", path = path, filename = "Bills.csv", coltypes = cols(.default = col_character(), Date = col_date(format = "%d-%m-%Y"), Amount = col_double()))
  initclients <- callModule(loadingOptions, "files", path = path, filename = "Clients.csv")
  initbillingaddresses <- callModule(loadingOptions, "files", path = path, filename = "Billing_Addresses.csv")
  initservices <- callModule(loadingOptions, "files", path = path, filename = "Services.csv", coltypes = cols(.default = col_character(), N_Service = col_integer(), Quantity = col_double(), Unit_price = col_double()))
  
  # Cost Estimates
  # --------------
  res <- callModule(editBills, "estimates", data = initestimates, servicesdata = initservices, clientsdata = clients, estimatesdata = NULL, path = path, filename = c("Estimates.csv", "Services.csv"), mode = "estimate")
  estimates <- reactive(res$data())
  services <- reactive(res$serv())
  
  # Bills
  # -----
  bills <- callModule(editBills, "bills", data = initbills, servicesdata = services, clientsdata = clients, estimatesdata = estimates, billingaddressesdata = billingaddresses, path = path, filename = "Bills.csv", mode = "bill")
  
  # Clients
  # -------
  clients <- callModule(editableDT, "clients", data = initclients, path = path, filename = "Clients.csv")#, ncol = 4)
  
  # Billing addresses
  # -----------------
  billingaddresses <- callModule(editableDT, "billingaddresses", data = initbillingaddresses, path = path, filename = "Billing_Addresses.csv")#, ncol = 4)
}