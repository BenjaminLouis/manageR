ui <- dashboardPage(
  #includeCSS("styles.css"),
  
  dashboardHeader(title = "ManageR"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("money-check")),
      menuItem("Estimates", tabName = "estimates", icon = icon("money-check")),
      menuItem("Bills", tabName = "bills", icon = icon("money-bill-wave")),
      menuItem("Budget", tabName = "budget", icon = icon("euro-sign")),
      menuItem("Clients", tabName = "clients", icon = icon("address-card")),
      menuItem("Billing addresses", tabName = "addresses", icon = icon("envelope")),
      menuItem("Missions", tabName = "missions", icon = icon("briefcase"))
    )
  ),
  
  dashboardBody(
    #useShinyjs(),
    tabItems(
      tabItem(
        tabName = "home",
        h4("Welcome to manageR !!"),
        wdLoadUI("wd"),
        br(),
        loadingOptionsInput("files")
      ),
      tabItem(
        tabName = "estimates",
        h4("Cost estimates management"),
        showDataUI("devis")
      ),
      tabItem(
        tabName = "bills",
        h4("Bills management")
      ),
      tabItem(
        tabName = "budget",
        h4("Budget management")
      ),
      tabItem(
        tabName = "clients",
        h4("Clients informations")
      ),
      tabItem(
        tabName = "addresses",
        h4("Billing addresses information")
      ),
      tabItem(
        tabName = "missions",
        h4("Missions information")
      )
    )
  )
)
