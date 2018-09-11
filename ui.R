ui <- dashboardPage(
  #includeCSS("styles.css"),
  
  ## HEADER ##
  ############
  dashboardHeader(title = "ManageR"),
  
  ## SIDEBAR ##
  #############
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Estimates", tabName = "estimates", icon = icon("money")),
      menuItem("Bills", tabName = "bills", icon = icon("bills")),
      menuItem("Budget", tabName = "budget", icon = icon("euro")),
      menuItem("Clients", tabName = "clients", icon = icon("address-card")),
      menuItem("Billing addresses", tabName = "addresses", icon = icon("envelope")),
      menuItem("Missions", tabName = "missions", icon = icon("briefcase"))
    )
  ),
  
  ## BODY ##
  ##########
  dashboardBody(
    #useShinyjs(),
    tabItems(
      
      # Home tab
      #----------
      tabItem(
        tabName = "home",
        h4("Welcome to manageR !!"),
        wdLoadUI("wd"),
        br(),
        loadingOptionsUI("files")
      ),
      
      # Cost estimates tab
      #-------------------
      tabItem(
        tabName = "estimates",
        h4("Cost estimates management"),
        tabsetPanel(
          selected = "Table",
          tabPanel(
            title = "Table",
            showDataUI("devis")
          ),
          tabPanel(
            title = "New cost estimate"
          ),
          tabPanel(
            title = "Modify a cost estimate"
          )
        )
      ),
      
      # Bills tab
      #----------
      tabItem(
        tabName = "bills",
        h4("Bills management"),
        tabsetPanel(
          selected = "Table",
          tabPanel(
            title = "Table",
            showDataUI("factures")
          ),
          tabPanel(
            title = "New bill"
          ),
          tabPanel(
            title = "Modify a bill"
          )
        )
      ),
      
      # Budget tab
      #-----------
      tabItem(
        tabName = "budget",
        h4("Budget management")
      ),
      
      # Clients tab
      #------------
      tabItem(
        tabName = "clients",
        h4("Clients informations"),
        tabsetPanel(
          selected = "Table",
          tabPanel(
            title = "Table",
            showDataUI("clients")
          ),
          tabPanel(
            title = "Add client",
            addDataUI("clients")
          )
        )
      ),
      
      # Billing adresses tab
      #---------------------
      tabItem(
        tabName = "addresses",
        h4("Billing addresses information"),
        showDataUI("facturations")
      ),
      
      # Missions tab
      #-------------
      tabItem(
        tabName = "missions",
        h4("Missions information")
      )
    )
  )
)
