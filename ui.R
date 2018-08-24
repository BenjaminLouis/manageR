ui <- dashboardPage(
  #useShinyjs(),
  #includeCSS("styles.css"),
  
  dashboardHeader(title = "ManageR"),
  
  dashboardSidebar(
    menuItem("Estimates", tabName = "estimates", icon = icon("money-check")),
    menuItem("Bills", tabName = "bills", icon = icon("money-bill-wave")),
    menuItem("Budget", tabName = "budget", icon = icon("euro-sign")),
    menuItem("Clients", tabName = "clients", icon = icon("address-card")),
    menuItem("Billing addresses", tabName = "addresses", icon = icon("envelope")),
    menuItem("Missions", tabName = "missions", icon = icon("briefcase"))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "estimates"),
      tabItem(tabName = "bills"),
      tabItem(tabName = "budget"),
      tabItem(tabName = "clients"),
      tabItem(tabName = "addresses"),
      tabItem(tabName = "missions")
    )
  )
)
