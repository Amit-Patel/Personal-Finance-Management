library(shiny)
library(RPostgreSQL)
library(session)
library(ggplot2)
library(scales)
library(shinyjs)
library(svDialogs)


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
    
    walletsquery <- c()
    X <<- c()
    Y <<- c()
    
    # piedetails <- c()
    userdetails <<- c()
    updateduserdetails <<- 0
    
    ui1 <- function(){ # function to update UI for login page
      
      
      tagList(
        div(id = "login",
            wellPanel(
              textInput("userName", "Username"),
              passwordInput("passwd", "Password"),
              br(),actionButton("Login", "Log in")))
        ,tags$style(type="text/css", "#login {font-size:15px;   text-align: centre;position:absolute;top: 40%;left: 50%;margin-top: 40px;margin-left: -170px;}")
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

       # ,tags$style(type="text/css", "#register {font-size:15px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: 40px;margin-left: -170px;}")
      )
      
    }
    
    ui3 <- function(){ # function to update UI for wallets initialization
      
      tagList(
        div(id = "wallets",
            wellPanel(
              offset=2,
              fluidRow(
                column(width=4,
                       offset=1,
                        useShinyjs(),
                        textInput("wallet_login","Username", value=""),
                        passwordInput("wallet_passwd","Password"),
                        #selectInput("wallets", "wallets",c("Credit","Debit","Cash"),multiple=TRUE),
                        selectInput("debit", "Debit", c("Yes", "No")),
                        textInput("debitamt", "Debit Balance"),
                        textInput("debitsav", "Debit Savings Amount"),
                        br(),actionButton("Initialize", "Initialize")),
                column(width=4,
                       offset=1,
                        selectInput("credit", "Credit", c("Yes", "No")),
                        textInput("creditamt", "Credit Balance"),
                        textInput("creditsav", "Credit Savings Amount"),
                        selectInput("cash", "Cash", c("Yes", "No")),
                        textInput("cashamt", "Cash Balance"),
                        textInput("cashsav", "Cash Savings Amount"))
                
              )))
            
        #,tags$style(type="text/css", "#wallets {font-size:15px;   text-align: left;position:relative;top: 40%;left: 50%;margin-top: 40px;margin-left: -170px;}")
      )
      
    }
    
    observeEvent(input$debit,{
      
      if(input$debit == "No"){
        disable("debitamt")
        disable("debitsav")
      }
      
      if(input$debit == "Yes"){
        enable("debitamt")
        enable("debitsav")
      }
      
    })
    
    observeEvent(input$credit,{
      
      if(input$credit == "No"){
        disable("creditamt")
        disable("creditsav")
      }
      
      if(input$credit == "Yes"){
        enable("creditamt")
        enable("creditsav")
      }
      
    })
    
    observeEvent(input$cash,{
      
      if(input$cash == "No"){
        disable("cashamt")
        disable("cashsav")
      }
      
      else{
        enable("cashamt")
        enable("cashsav")
      }
      
    })
    
    observe({
      
      if(!is.null(input$Register)){
        if(input$Register > 0){
      
            if((input$username!="" && input$password!="" && input$email!="" && input$phno!="")){
              
              con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
              dbSendQuery(con,paste0("Insert into user_table values ('",input$username,"','",input$password,"','",input$email,"',",input$phno,",","0,0)"))
              userinfo <<- dbGetQuery(con, "select user_name, u_id, password from user_table")
              newuserid <<- dbGetQuery(con,"select u_id from user_table order by u_id desc limit 1")
              print(newuserid)
              dbDisconnect(con)
              
              registered <<- 1
                
              my_username <<- userinfo$user_name
              my_password <<- userinfo$password
                
              }    
            else
            {
              user <- dlgMessage("Please enter valid details for registration.")
            }
        
       }
        }
    })
    
    observe({
      
      if(!is.null(input$Initialize)){
        if(input$Initialize > 0){
          
          if((input$debit=="Yes") && (input$debitamt!="") && (input$debitsav!="")){
            
            print("DEBIT SUCCESS")
            con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
            dbSendQuery(con,paste0("Insert into wallet_table values (",2,",",userinfo$u_id[which(input$wallet_login==my_username)],",'Debit',",input$debitamt,",'Rupees',",input$debitsav,")"))
            dbDisconnect(con)
            flag <- 1
          }    
          else
          {
            print("DEBIT INITIALIZED")
          }
          
          if((input$credit=="Yes") && (input$creditamt!="") && (input$creditsav!="")){
            
            print("CREDIT SUCCESS")
            con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
            dbSendQuery(con,paste0("Insert into wallet_table values (",1,",",userinfo$u_id[which(input$wallet_login==my_username)],",'Credit',",input$creditamt,",'Rupees',",input$creditsav,")"))
            dbDisconnect(con)
            flag <- 1
          }    
          else
          {
            print("CREDIT INITIALIZED")
          }
          
          if((input$cash=="Yes") && (input$cashamt!="") && (input$cashsav!="")){
            
            print("CASH SUCCESS")
            con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
            dbSendQuery(con,paste0("Insert into wallet_table values (",3,",",userinfo$u_id[which(input$wallet_login==my_username)],",'Cash',",input$cashamt,",'Rupees',",input$cashsav,")"))
            dbDisconnect(con)
            flag <- 1
          }    
          else
          {
            print("CASH INITIALIZED")
          }
          
          if(flag!=1){
            user <- dlgMessage("Please enter valid details.")
            
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
      
      
      con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
      userinfo <<- dbGetQuery(con, "select user_name, u_id, password from user_table")
      dbDisconnect(con)
      
      print(userinfo)
      print(input$wallet_login)
      print(which(input$wallet_login==userinfo$user_name))
      print(which(input$wallet_passwd==userinfo$password))
      
      if (!is.null(input$wallet_login) && length(which(input$wallet_login==userinfo$user_name))>0 && length(which(input$wallet_passwd==userinfo$password))>0)
     { if(which(input$wallet_login==userinfo$user_name)==which(input$wallet_passwd==userinfo$password))
         {
         enable("Initialize")
      
         }

      else{
        disable("Initialize")
        user <- dlgMessage("Please enter valid details.")
        
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
                      div(class="outer",do.call(bootstrapPage,c("",ui2()))))
                    ),
                  tabItem(
                    tabName = "wallets",
                    box(
                      div(class="outer",do.call(bootstrapPage,c("",ui3()))))
                  )
                  )
                })
                
        
        output$side <- renderUI({
          side <- sidebarMenu(
            menuItem("Login", tabName = "login", icon = icon("login")),
            menuItem("Register", tabName = "register", icon = icon("register")),
            menuItem("Wallets", tabName = "wallets", icon = icon("wallets"))
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
        # user <- dlgMessage("Error!")
        con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
        userdetails <<- dbGetQuery(con, paste0("select a.type, a.u_id, a.w_id, a.amount, a.date from transaction_table a, wallet_table b where a.w_id = b.w_id and a.u_id = b.u_id and b.u_id =", userinfo$u_id[Id.username]) )
        print(userdetails)
        print(paste0("select w_type as Wallet,amount as Balance, savings as Savings from wallet_table where u_id=",userinfo$u_id[Id.username]))
        walletsquery <<- dbGetQuery(con, paste0("select w_type as Wallet,amount as Balance, savings as Savings from wallet_table where u_id=",userinfo$u_id[Id.username]))
        updateSelectInput(session, "ttype", "Transaction type", choices = c("ALL",unlist(userdetails$type)))
        updateSelectInput(session, "ttype1", "Transaction type", choices = c("ALL",unlist(userdetails$type)))
        updateSelectInput(session, "wallet_type", choices = c(walletsquery$wallet))
        
        updateCheckboxGroupInput(session,"icons",choices = c(walletsquery$wallet))
    
        print(walletsquery)
        dbDisconnect(con)
        

      }
    })
    
    observe({
      if(!is.null(input$Update)){
        if(input$Update > 0){
          if(!is.null(input$icons) && input$icons!="" && input$updatedamt!="" && input$updatedsav!="")
          {print(paste0("Update wallet_table set amount=",input$updatedamt," and savings=",input$updatedsav," where u_id=",userinfo$u_id[Id.username]," and w_type='", input$icons,"'"))
          con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
          dbSendQuery(con,paste0("Update wallet_table set amount =",input$updatedamt,",savings=",input$updatedsav," where u_id=",userinfo$u_id[Id.username]," and w_type='", input$icons,"'"))
          walletsquery <<- dbGetQuery(con, paste0("select w_type as Wallet,amount as Balance, savings as Savings from wallet_table where u_id=",userinfo$u_id[Id.username]))
          dbDisconnect(con)
          
          output$wallets <- renderTable({walletsquery},spacing = "l", width = 100)
          }
          else{
            user <- dlgMessage("Please enter valid details.")
            
          }
        }
      }
    })
    
    observe({
      if(!is.null(input$Delete)){
        if(input$Delete > 0){
          print(input$pswd)
          print(input$confirmpswd)
          print(my_password)
          if(!is.null(input$icons) || input$icons!="" && input$pswd!="" && input$confirmpswd!="" && input$pswd==my_password && input$pswd==input$confirmpswd)
          {print(paste0("Delete from wallet_table where w_type='",input$icons,"' and u_id =",userinfo$u_id[my_password]))
            con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
            dbSendQuery(con,paste0("Delete from wallet_table where w_type='",input$icons,"' and u_id =",userinfo$u_id[Id.password]))
            walletsquery <<- dbGetQuery(con, paste0("select w_type as Wallet,amount as Balance, savings as Savings from wallet_table where u_id=",userinfo$u_id[Id.username]))
            userdetails <<- dbGetQuery(con, paste0("select a.type, a.u_id, a.w_id, a.amount, a.date from transaction_table a, wallet_table b where a.w_id = b.w_id and a.u_id = b.u_id and b.u_id =", userinfo$u_id[Id.username]) )
            piedetails <<- dbGetQuery(con, paste0("select sum(amount), type from (select * from transaction_table where u_id=", userinfo$u_id[Id.username],") a group by type") )
            
            dbDisconnect(con)
            
            updateSelectInput(session, "ttype", "Transaction type", choices = c("ALL",unlist(userdetails$type)))
            updateSelectInput(session, "ttype1", "Transaction type", choices = c("ALL",unlist(userdetails$type)))
            updateSelectInput(session, "wallet_type", choices = c(walletsquery$wallet))
            updateCheckboxGroupInput(session,"icons",choices = c(walletsquery$wallet))
            
            output$wallets <- renderTable({walletsquery},spacing = "l", width = 100)
            
            output$UI_plot <- reactivePlot(function(){
              
              withProgress(message = "Rendering main Timeline chart", value = 0, {
                plotTimeWindow(c(piedetails$sum), c(piedetails$type))
              })
            })
            
          }
          else{
            user <- dlgMessage("Please enter valid details.")
            
          }
        }
      }
    })
    
    observe({
      
      if(!is.null(input$actions) && input$actions=="Update Wallet"){
        disable("pswd")
        disable("confirmpswd")
        disable("Delete")
        enable("updatedamt")
        enable("updatedsav")
        enable("Update")
      }
      
      else{
        disable("updatedamt")
        disable("updatedsav")
        disable("Update")
        enable("pswd")
        enable("confirmpswd")
        enable("Delete")
      }
    })
    
    #GATHERING PIE CHART INFO
    observe({
      
      if(USER$Logged == TRUE)
      {
        
        con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
        piedetails <<- dbGetQuery(con, paste0("select sum(amount), type from (select * from transaction_table where u_id=", userinfo$u_id[Id.username],") a group by type") )
        dbDisconnect(con)
        
        
      }
    })
    
    output$wallets <- renderTable({walletsquery},spacing = "l", width = 100)
    
 
    observe({
      
      if(!is.null(input$Confirm)){
        if(input$Confirm > 0){
          
          if((input$amount!="") && (input$date!="") && (input$location!="") && (input$lender!="") && (input$borrower!="")){
            
            if(input$status=="Completed"){
              boolvar <- TRUE
            }
            else
              
            {
              user <- dlgMessage("Please enter valid details.")
              boolvar <- FALSE
            }
            
            if(input$wallet_type=="Credit"){
              wallet_id <<- 1
            }
            
            if(input$wallet_type=="Debit"){
              wallet_id <<- 2
            }
            if(input$wallet_type=="Cash"){
              wallet_id <<- 3
            }
            
            con <<- dbConnect(PostgreSQL(), user = database_user, dbname = database_name, password = database_password)
            dbSendQuery(con,paste0("Insert into transaction_table (w_id, u_id, type, amount, date, location,status, lender, borrower) values (",wallet_id,",",userinfo$u_id[Id.username],",'",input$trans_type,"',",input$amount,",'",input$date,"','",input$location,"',",boolvar,",'",input$lender,"','",input$borrower,"')"))
            dbSendQuery(con,paste0("Update wallet_table set amount = amount-",input$amount," where u_id=",userinfo$u_id[Id.username]," and w_id =",wallet_id))
            print(paste0("Update table transaction_table set amount = amount-",input$amount," where u_id=",userinfo$u_id[Id.username]," and w_id = ",wallet_id))
            
            walletsquery <<- dbGetQuery(con, paste0("select w_type as Wallet,amount as Balance, savings as Savings from wallet_table where u_id=",userinfo$u_id[Id.username]))
            userdetails <<- dbGetQuery(con, paste0("select a.type, a.u_id, a.w_id, a.amount, a.date from transaction_table a, wallet_table b where a.w_id = b.w_id and a.u_id = b.u_id and b.u_id =", userinfo$u_id[Id.username]) )
            print(userdetails)
            updateSelectInput(session, "ttype", "Transaction type", choices = c("ALL",unlist(userdetails$type)))
            updateSelectInput(session, "ttype1", "Transaction type", choices = c("ALL",unlist(userdetails$type)))
            updateSelectInput(session, "wallet_type", choices = c(walletsquery$wallet))
            
            piedetails <<- dbGetQuery(con, paste0("select sum(amount), type from (select * from transaction_table where u_id=", userinfo$u_id[Id.username],") a group by type") )
            dbDisconnect(con)
            
            output$wallets <- renderTable({walletsquery},spacing = "l", width = 100)
  
            
            
            output$UI_plot <- reactivePlot(function(){
              
              withProgress(message = "Rendering main Timeline chart", value = 0, {
                plotTimeWindow(c(piedetails$sum), c(piedetails$type))
              })
            })
          }    
          else
          {
            user <- dlgMessage("Please enter valid details.")
          }
          
        }
      }
    })
    
    
    #REACTIVE CALL TO PIECHART FUNCTION
    observeEvent(piedetails,{
      output$UI_plot <- reactivePlot(function(){
        
        withProgress(message = "Rendering main Timeline chart", value = 0, {
          plotTimeWindow(c(piedetails$sum), c(piedetails$type))
        })
      })
    })
    
    
    
    
    #PIE CHART FUNCTION PLOT
    plotTimeWindow<-function(xs, ys){
      
      if(length(xs)==0){
          user <- dlgMessage("You have no transactions in your account.")
        }
        
      else{
      
          df <- data.frame(
            categories = ys,
            proportion = xs
          )
          print("calling plot function")
          
          output$pievalues <- renderTable({
            
            data.frame(
              Category = c(unlist(ys)),
              Expense = as.character(c(xs)),
              Proportion = c(xs*100/sum(xs)),
              stringsAsFactors = FALSE)
          })
          
          print(df)
          ggplot(df, aes(x="", y=proportion, fill=categories))+geom_bar(width = 2, stat = "identity")+
            coord_polar("y", start=0)+
            scale_fill_brewer(palette="Blues")+
            theme_minimal()
            #+
            #geom_text(aes(y = value/length(df$value) + c(0, cumsum(value)[-length(value)]), 
            #              label = percent(value/100)), size=5)
      }
    }
    

      
    
    
      output$category_plot <- reactivePlot(function(){
        
        if(length(userdetails$amount)==0){
          user <- dlgMessage("You have no transactions in your account.")
        }
        else{
              if(input$ttype1 == "ALL"){
                categoried <- 1:length(userdetails$amount)
              }
              
              else{
                
                categoried <- which(userdetails$type == input$ttype1)
              }
              
              X <<- c(unlist(userdetails$amount[categoried]))
              Y <<- as.Date(c(unlist(userdetails$date[categoried])),'%m/%d/%Y')
              
              A <-(which(Y>=input$start_date))
              B <-(which(Y<=input$end_date))
              print(intersect(B,A))
              
              X <<- X[intersect(B,A)]
              Y <<- Y[intersect(B,A)]
              
              df <- data.frame(Y,X)
              #df <- df[intersect(B,A),]
              df <- df[with(df, order(Y)),]
      
              ggplot(data = df,aes(Y, X), type = "b", color = "blue")+
                geom_line()+
                geom_point(shape=18,fill="blue", color="darkred", size=3)
            
       
              # output$print_box1 <- renderText({paste("The average expenditure is:",mean(X))})
              # output$print_box2 <- renderText({paste("Number of transactions:",length(X))})
              # output$print_box3 <- renderText({paste("Highest amount transaction amount is:",max(X))})
              # output$print_box4 <- renderText({paste("Lowest amount transaction amount is:",min(X),"\n")})
        }            
        
        
        
        
        
      
      })
    
    
      observeEvent({input$start_date 
                    input$end_date},{
        

                      output$values <- renderTable({
                        
                          data.frame(
                            Name = c("Average expenditure:",
                                     "Transactions:",
                                     "Max:",
                                     "Min:"),
                            Value = as.character(c(mean(X),
                                                   length(X),
                                                   max(X),
                                                   min(X))),
                            stringsAsFactors = FALSE)
                      })
      
      })
      
    
      output$main_plot <- reactivePlot(function(){
       # print(input$ttype)
       # print(which(userdetails$type == input$ttype))
       # print(userdetails[which(userdetails$type == input$ttype)])
        
        if(length(userdetails$amount)==0){
          user <- dlgMessage("You have no transactions in your account.")
        }
        
        else{
        
            if(input$ttype != "ALL")
            
            {
              updateduserdetails <<- which(userdetails$type == input$ttype)
            }
            
            else
            {   
              updateduserdetails <<- 1:length(userdetails$type)
            }
            
            B <- c(unlist(userdetails$amount[updateduserdetails]))
            barplot(B, main="User Transactions", xlab="Transaction Id", ylab="Amount", names.arg=c(userdetails$t_id[updateduserdetails]),
                    border="Blue" )
        }
      })
      
    
    observeEvent(input$ttype,{   
      output$net_expense <- renderText({
        paste0("The net expenditure on ",input$ttype," transactions is :",sum(userdetails$amount[updateduserdetails]))
      })
    })
      
      
  })
  

