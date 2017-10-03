library(shinydashboard)
library(leaflet)

test_title="Dashboard Visualization of Critical Situation Monitoring"

# side menu on the dashboard, logout button logs out user
test_side=list(sidebarMenu(
  menuItem("Expenses", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Proportions", tabName = "secondary", icon = icon("secondary")),
  menuItem("Logout", tabName = "logout", icon = icon("logout"))
  ))

test_main=list(
  
  tabItems(
    tabItem(tabName = "logout",
            column(width = 10,
            offset = 2,
            h4("Are you sure you want to log out?"),
            actionButton('Logout', 'Logout')
            )
    ),
    tabItem(tabName = "dashboard",
            column(width = 10,#class = "well",
            offset = 1,
            selectInput("ttype", "Transaction type", c("NULL")),
            plotOutput(outputId = "main_plot"))
            ),
    tabItem(tabName = "secondary",
            
            column(width = 12, #class = "well",
                   h4("Pie chart of transactions"),
                   plotOutput(outputId = "UI_plot")
                )
            )
  ))
