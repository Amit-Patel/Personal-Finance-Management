library(shiny)
library(RPostgreSQL)
library(session)
library(ggplot2)
library(scales)

source("user.R")

  
  database_name <<- "mydb" 
  database_password <<- "admin"
  database_user <<- "postgres"
  
    con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
    userinfo <<- dbGetQuery(con, "select user_name, u_id, password from user_table")
    dbDisconnect(con)
    
    my_username <<- userinfo$user_name
    my_password <<- userinfo$password
    
    print (my_username)
    print (my_password)
  
  
  get_role=function(user){
    
    if(user=='admin') {
      
      return('admin')
    }else{
      
      return("")
    }
  }
  
  get_ui=function(role){
    
    itog=list()
    if(role == 'adin')
    {
      itog$title=admin_title
      itog$main=admin_main
      itog$side=admin_side
      return(itog)
    }
    
    itog$title=test_title
    itog$main=test_main
    itog$side=test_side
    return(itog)
  }
  
  
  shinyServer(function(input, output,session) {
    
    USER <- reactiveValues(Logged = FALSE,role=NULL, type = NULL)
 
    
    ui1 <- function(){ # function to update UI for login page
      
      tagList(
        div(id = "login",
            wellPanel(
              textInput("userName", "Username"),
              passwordInput("passwd", "Password"),
              br(),actionButton("Login", "Log in")))
        ,tags$style(type="text/css", "#login {font-size:15px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: 40px;margin-left: -170px;}")
      )
      
    }
    
    ui2 <- function(){ # function to update UI for login page
      
      tagList(
        div(id = "register",
            wellPanel(
              textInput("name", "Name"),
              textInput("email", "Email"),
              textInput("username", "Username"),
              passwordInput("password", "Password"),
              textInput("phno", "Phone Number"),
              br(),actionButton("Register", "Register")))
        ,tags$style(type="text/css", "#login {font-size:15px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: 40px;margin-left: -170px;}")
      )
      
    }
    
    observe({
      
      if(!is.null(input$Register)){
        if(input$Register > 0){
      
      if((input$name!="") && (input$email!="") && (input$username!="") && (input$password!="") && (input$phno!="")){
        
        con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
        dbSendQuery(con,paste0("Insert into user_table values ('",input$username,"','",input$password,"','",input$email,"',",input$phno,",","0,0)"))

        userinfo <<- dbGetQuery(con, "select user_name, u_id, password from user_table")
        dbDisconnect(con)
          
        my_username <<- userinfo$user_name
        my_password <<- userinfo$password
          
        }    
      else
      {
        print("NULL")
      }
        
      }
        }
    })
    
    
    observe({ 
      if (USER$Logged == FALSE) {
        
        if (!is.null(input$Login)) {
          if (input$Login > 0) { # true if login button is pressed
            
            Username <- isolate(input$userName) 
            Password <- isolate(input$passwd)
            Id.username <<- which(my_username == Username)
            Id.password <<- which(my_password == Password)
            
            if (length(Id.username) > 0 & length(Id.password) > 0) {
              if (Id.username == Id.password) {
                USER$Logged <- TRUE
                USER$role=get_role(Username)
                USER$type <<- userinfo$user_type[Id.username]
                
              }
              
              else{
                USER$role = 'invalid'
              }
            }
            else{
              USER$role = 'invalid'
            }
            
          }
        }
      }
    })
    
    observe({
      
      if (USER$Logged == FALSE|(!is.null(input$Logout)&&input$Logout > 0)) {
        
        
        output$title <- renderText({
          title = 'Login Page'
        })
        
        output$page <- renderUI({
          tabItems(
            
          #Login Tab
          tabItem(
            tabName = "login",
              box(
                div(class="outer",do.call(bootstrapPage,c('Please enter your user credentials.',ui1()))))
            ),
          
          #Registration Tab
          tabItem(
            tabName = "register",
            box(
              div(class="outer",do.call(bootstrapPage,c('Please enter your information.',ui2()))))
            )
          )
        })
        
        output$side <- renderUI({
          side <- sidebarMenu(
            menuItem("Login", tabName = "login", icon = icon("login")),
            menuItem("Register", tabName = "register", icon = icon("register"))
          )

        })
      }
      
      if(!is.null(USER$role)&& USER$role == 'invalid')
      {
        output$error <- renderUI({
          error = 'Enter correct credentials'}) 
      }
      
      if (USER$Logged == TRUE & is.null(input$Logout))    {
        itog=get_ui(USER$role)
        output$title<- renderText({
          itog$title
        })
        output$side <- renderUI({
          itog$side
        })
        output$page <- renderUI({
          itog$main
        })
        output$error <- renderText({
          error = NULL
        })
      }
    })
    
    
    
    #DETAILS OF USER THAT IS LOGGED IN
    observe({
      
      if(USER$Logged == TRUE)
      {
        
        con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
        userdetails <<- dbGetQuery(con, paste0("select * from transaction_table where w_id =", userinfo$u_id[Id.username]) )
        #print(userdetails)
        updateSelectInput(session, "ttype", "Transaction type", choices = c("ALL",unlist(userdetails$type)))  
        
        dbDisconnect(con)


      }
    })
    
    #GATHERING PIE CHART INFO
    observe({
      
      if(USER$Logged == TRUE)
      {
        
        con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
        piedetails <<- dbGetQuery(con, paste0("select sum(amount), type from (select * from transaction_table where w_id=", userinfo$u_id[Id.username],") a group by type") )
        dbDisconnect(con)
        
        
      }
    })
    
    
    #REACTIVE CALL TO PIECHART FUNCTION
    observe({
      output$UI_plot <- reactivePlot(function(){
        
        withProgress(message = "Rendering main Timeline chart", value = 0, {
          plotTimeWindow(c(piedetails$sum), c(piedetails$type))
        })
      })
    })
    
    
    #PIE CHART FUNCTION PLOT
    plotTimeWindow<-function(xs, ys){
      df <- data.frame(
        categories = ys,
        proportion = xs
      )
      print("calling plot function")
      
      print(df)
      ggplot(df, aes(x="", y=proportion, fill=categories))+geom_bar(width = 2, stat = "identity")+
        coord_polar("y", start=0)+
        scale_fill_brewer(palette="Blues")+
        theme_minimal()
        #+
        #geom_text(aes(y = value/length(df$value) + c(0, cumsum(value)[-length(value)]), 
        #              label = percent(value/100)), size=5)
      
    }
    
    
    
    
    
      
    
      
      output$main_plot <- reactivePlot(function(){
       # print(input$ttype)
       # print(which(userdetails$type == input$ttype))
       # print(userdetails[which(userdetails$type == input$ttype)])
        
        if(input$ttype != "ALL")
        
        {
          updateduserdetails <- which(userdetails$type == input$ttype)
        }
        else
       {   updateduserdetails <- 1:length(userdetails)
      }
        B <- c(unlist(userdetails$amount[updateduserdetails]))
        barplot(B, main="User Transactions", xlab="Transaction Id", ylab="Amount", names.arg=c(userdetails$t_id[updateduserdetails]),
                border="Blue" )
      })
      
      
  })
  

