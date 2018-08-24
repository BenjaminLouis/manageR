ui <- fluidPage(useShinyjs(),
                #includeCSS("styles.css"),
                
                # App title ----
                titlePanel("ManageR"),
                
                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(
                    
                    # Working directory choice
                    h4("Working directory")

                  ),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    # Output: Tabset w/ plot, summary, and table ----
                    tabsetPanel(type = "tabs",
                                tabPanel("Estimates"

                                ),
                                tabPanel("Invoices"
                                         
                                ),
                                tabPanel("Budget"
                                         
                                ),
                                tabPanel("Clients"
                                         
                                ),
                                tabPanel("Billing addresses"
                                         
                                ),
                                tabPanel("Missions"
                                         
                                )
                    )
                    
                  )
                )
                
)