library(shinydashboard)
library(leaflet)

test_title="Dashboard Visualization of Critical Situation Monitoring"

# side menu on the dashboard, logout button logs out user
test_side=list(sidebarMenu(
  menuItem("Expenses", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Proportions", tabName = "secondary", icon = icon("secondary")),
  menuItem("Enter Transactions", tabName = "tertiary", icon = icon("tertiary")),
  menuItem("Wallets", tabName = "qnary", icon = icon("qnary")),
  menuItem("History", tabName = "history", icon = icon("history")),
  menuItem("Logout", tabName = "logout", icon = icon("logout"))
  ))

test_main=list(
  
  
  tabItems(
    
    tabItem(tabName = "history",
            
            fluidRow(
              
                column(width=3,
                    offset = 1,
                    selectInput("ttype1", "Transaction type", c("NULL"))),
                column(width = 3,
                    dateInput("start_date", "Start Date:", value = "2001-02-29")),
                column(width = 3,
                       dateInput("end_date", "End Date:", value = "2020-02-29"))
               
            ),
            
            column(width = 10,
                   offset = 1,
                  tableOutput("values"),
                  plotOutput(outputId = "category_plot"))
               
    ),
  
    tabItem(tabName = "logout",
            column(width = 10,
            offset = 2,
            h4("Are you sure you want to log out?"),
            actionButton('Logout', 'Logout')
            )
    ),
  
      
    tabItem(tabName = "tertiary",
              column(width = 10,
                     offset = 2,
                     selectInput("wallet_type", "Wallet type", c("Credit", "Debit", "Cash")),
                     selectInput("trans_type", "Transaction type", c("FOOD","BILLS","PHONE","TRANSPORT","SHOPPING","ENTERTAINMENT","TRAVEL","GROCERY","BUSINESS","PERSONAL","LOAN")),
                     textInput("amount","Amount"),
                     textInput("date","Date"),
                     textInput("location","Location"),
                     selectInput("status","Status",c("Completed","Pending")),
                     textInput("lender","Lender"),
                     textInput("borrower","Borrower"),
                     br(),actionButton("Confirm", "Confirm"))
    
      ),
    tabItem(tabName = "dashboard",
            column(width = 10,#class = "well",
            offset = 1,
            selectInput("ttype", "Transaction type", c("NULL")),
            plotOutput(outputId = "main_plot"),
            textOutput("net_expense"))
            ),
    tabItem(tabName = "secondary",
            
            column(width = 12, #class = "well",
                   h4("Pie chart of transactions"),
                   plotOutput(outputId = "UI_plot")),
            column(width = 12,
                   offset = 4,
                   tableOutput("pievalues"))
            ),
  
    tabItem(tabName = "qnary",
            
            column(width = 5, #class = "well",
                   offset = 1,
                   h4("Status of wallets possesed by user:"),
                   tableOutput("wallets"),
                   checkboxGroupInput("icons","Select wallet:",
                                      c("Credit","Debit","Cash")),
                   selectInput("actions","Actions:", c("Update Wallet", "Delete Wallet")),
                   fluidRow(
                      column(
                          width=5,
                          textInput("updatedamt","Updated Wallet Balance", value=NULL),
                          textInput("updatedsav","Updated Savings Amount", value=NULL),
                          br(),actionButton("Update", "Update")
                        ),
                      column(
                        width=4,
                        offset=2,
                        passwordInput("pswd","Password:", value=""),
                        passwordInput("confirmpswd","Confirm Password:", value=""),
                        br(),actionButton("Delete", "Delete")
                        
                      )
                   ),
                   textOutput("txt")
                   
                   )
            )
  ))
