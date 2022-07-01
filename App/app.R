library(shiny)
library(shinydashboard)
library(plotrix)
library(plotly)
library(RColorBrewer)
library(gganimate)
library(tidyverse)
library(reshape2)
library(shinythemes)
library(dashboardthemes)


ui <- dashboardPage(skin = 'blue',
                    dashboardHeader(title = 'STAT 6341 Project Dashboard',titleWidth = 290),
                    dashboardSidebar(width = 290,
                        sidebarMenu(
                            menuItem("Overview",tabName = 'overview',icon = icon('home')),
                            menuItem("Data Table",tabName = 'data',icon = icon('cog')),
                            menuItem("Interactive Plots",tabName = 'demo',icon = icon('chart-line')),
                            menuItem("Compare Performance",tabName = "compare",icon = icon('balance-scale')),
                            menuItem("Predict Points Scored", tabName = 'choose_plot', icon = icon('lightbulb-o')),
                            menuItem("Help Page",tabName = "help",icon = icon('book-reader'))
                                    )),
                    dashboardBody(shinyDashboardThemes(theme = "poor_mans_flatly"),
                           tabItems(
                              tabItem(tabName='overview',
                                    h1('Overview'),
                                      fluidRow(
                                         box(verbatimTextOutput("welcome"), tags$head(tags$style(HTML("
                                 #welcome { font-size: 17px;}
                            "))),width = 12),
                                         column(img(src = "pic3.jpg", height = 330, width = 530),width = 2)
                                              ),
                                     ),
                              tabItem(tabName='data',
                                    h3('Please upload your data here.'),
                                    fluidPage(
                                        # Create a new Row in the UI for selectInputs
                                        fluidRow(
                                           fileInput("file", label = "upload a csv file"),
                                                ),
                                             ),
                                        # Create a new row for the table.
                                         DT::dataTableOutput("input_file")
                                       ),
                            tabItem(tabName='demo',
                                    h1('Interactive Plots'),
                                    fluidPage(
                                       sidebarPanel(
                                          selectInput("stat","Choose from below", choices =list("PTS","FG_percent","ThreeP_percent","PlusMinus")),width = 3),
                                       mainPanel(
                                          fluidRow(
                                             plotlyOutput("plot3"), width = 6,status = "info",solidHeader = TRUE),
                                          fluidRow(
                                           plotlyOutput("plot1"), title = 'Wins/Losses by Year', width = 6,status = "info",solidHeader = TRUE),
                                          fluidRow(
                                           plotlyOutput("plot2"), title = 'Personal Points by Minutes Played', width = 6,status = "info",solidHeader = TRUE)
                                                )
                                             )),
                            tabItem(tabName='choose_plot',
                                    h1('Predict Points Scored'),
                                    fluidPage(
                                      sidebarPanel(
                                        selectInput("opp", "Opponent Team:",""),
                                        sliderInput("mp", "Minutes Played", min = 0, max = 50, step=5,value = 20),
                                        sliderInput("stl", "Steals", min = 0, max = 8, value = 5),
                                        sliderInput("fga", "Field Goal Attempts",min = 1,max = 30,value = 15),
                                        
                                        actionButton(inputId = "go", label = "Update")
                                                   ),
                                      mainPanel(
                                        fluidRow(
                                          plotlyOutput('trendPlot', height = "500px")
                                          ),
                                        fluidRow(
                                          column(width=6, offset=3, div(tableOutput("values"), style = "font-size:150%"))
                                        )
                                      )
                                    )
                                  ),
                            tabItem(tabName = "compare",
                                    h1('Compare Game Performance by Year'),
                                    fluidPage(titlePanel(
                                              mainPanel(
                                                width = 12,
                                                splitLayout(
                                                  cellWidths = c("50%", "50%"),
                                                  div(
                                                    selectInput( input = "year1", label = "Choose a year", ""),
                                                    plotOutput("plot4")
                                                   ),
                                                  div(
                                                    selectInput(input = "year2", label = "Choose a year", ""),
                                                    plotOutput("plot5")
                                                    )
                                                 ),
                                                tableOutput("stat.table")
                                              )))),
                            tabItem(tabName = "help",fluidPage(
                               HTML("<p>The data source is <a href='https://www.basketball-reference.com'>Basketball-Reference.com</a>
                                    <br>
                                       You will find Game Logs under the tab Players.You can scrape the player data you want from the website.
                                    <br>
                                       The data should look like the one below. Make sure to remove any redundant rows and columns.<p>"),
                                    fluidRow(
                                       column(img(src = "excel1.png", height = 200),width = 10)),
                                    fluidRow(
                                       column(img(src = "excel2.png", height = 200, ),width = 8))
                            )))
                        )) 

server <- function(input,output,session) {
  
   #### Read data ####
   newdata <- reactive({
      req(input$file)
      file <- input$file
      if(is.null(file)) {return()}
      data <-  read.csv(file$datapath, header = TRUE)
      
      #### data preprocessing ####
      # covert cols to numeric
      data[,c(8:27)] <- sapply(data[,c(8:27)], as.character)
      data[,c(8:27)] <- sapply(data[,c(8:27)], as.numeric)
      # replace NA with 0
      data[,c(8:27)][is.na(data[,c(8:27)])] <- 0
      # change variable names： 3P,3PA,3P_percent,+.-
      colnames(data)[c(11,12,13)] <- c("ThreeP","ThreePA","ThreeP_percent")
      colnames(data)[27] <- c("PlusMinus")
      # replace all / with . and all % with _percent
      colnames(data) <- colnames(data) %>%
         str_replace("[.]", "_percent")
      # convert the character values in the MP column to actual minutes played
      game_min <- as.numeric(sub(":.*", "", data$MP))
      game_sec <- as.numeric(sub(".*:", "", data$MP))
      data$MP <- game_min+game_sec/60
      data$MP[is.na(data$MP)] <- 0
      data <- data %>% 
         mutate_at(vars(MP), list(~ round(., 2)))
      # GS col:replace missing values with None
      data$GS <- as.factor(data$GS)
      levels(data$GS) <- list("0"="0","1"="1")
      level_GS <- levels(data$GS)
      level_GS[length(level_GS) + 1] <- "None"
      data$GS <- factor(data$GS, levels = level_GS)
      data$GS[is.na(data$GS)] <- 'None'
      # add outcome varaibles
      data$WL <- ifelse(grepl("W", data$Outcome), "W", "L")
      data <- data[,-5]
      data$WL <- as.factor(data$WL)
      # add Year variable
      data$Year <- str_sub(data$Date,1,4)
      data <- data[,-1]
      data
   })

   output$input_file <- DT::renderDataTable({
      DT::datatable(newdata(),options = list(scrollX = TRUE))
   })
  output$welcome <- renderText({
    paste("Welcome to the Project Dashboard for STAT 6341 Sports Analytics! 
    
The goal of dashboard is to visualize NBA player statistics.
         
   - The Data Table page allows you to upload CSV file and view player data.
   - In the Interactive Plots page, you will explore data visualization regarding to the player statistics.
   - The Compare Performance page allows you to compare player's game performance by year.
   - The Predict Points Scored page allows you to preidct player's points by inputting some variables.
   - The Help Page is a brief guide to obtain the data that meets the requirement for this dashboard.

          
If you have any questions, please contact Xiaoyingzi Liu at xiaoyingzil@smu.edu")
  })
  
  output$plot1 <- renderPlotly({
    wins <- newdata() %>% 
        select(Year,WL) %>%
        group_by(Year,WL) %>%
        count()
        
    plot_ly(wins, x = ~Year, y = ~n, type="bar", color = ~WL) %>%
              layout(xaxis = list(tickmode = "linear"), yaxis=list(title="Number of Wins/Losses"))
            
  })
  
   output$plot2 <- renderPlotly({
    newdata() %>%
    plot_ly( x= ~PTS, y= ~MP, color = ~WL, frame = ~Year,text = ~WL,hoverinfo = "text", type = 'scatter',mode = 'markers') %>%
    layout(xaxis=list(title="Personal Points"), yaxis=list(title="Minutes Played")
           )
  })
  
   output$plot3 <- renderPlotly({
      if(input$stat == "FG_percent"){
         bar <- newdata() %>% select(Opp,FG_percent) %>%
            group_by(Opp) %>% summarise_all(funs(mean)) %>% plot_ly(
               x=~Opp, y=~FG_percent,type="scatter",mode="lines+markers") %>%
            layout(xaxis=list(tickangle = 45))
      }
      if(input$stat == "ThreeP_percent"){
         bar <- newdata() %>% select(Opp,ThreeP_percent) %>%
            group_by(Opp) %>% summarise_all(funs(mean)) %>% plot_ly(
               x=~Opp, y=~ThreeP_percent,type="scatter",mode="lines+markers")%>%
            layout(xaxis=list(tickangle = 45))
      }
      if(input$stat == "PTS"){
         bar <- newdata() %>% select(Opp,PTS) %>%
            group_by(Opp) %>% summarise_all(funs(mean)) %>% plot_ly(
               x=~Opp, y=~PTS,type="scatter",mode="lines+markers") %>%
            layout(xaxis=list(tickangle = 45))
      }
      else if(input$stat == "PlusMinus"){
         bar <- newdata() %>% select(Opp,PlusMinus) %>%
            group_by(Opp) %>% summarise_all(funs(mean)) %>% plot_ly(
               x=~Opp, y=~PlusMinus,type="scatter",mode="lines+markers") %>%
            layout(xaxis=list(tickangle = 45))
      }
      bar
   })
   
    output$trendPlot <- renderPlotly({
     
     fig <- plot_ly(newdata(), x = ~MP, y = ~STL, z = ~FGA,color = ~WL, marker=list(size=5),
                    text = paste("Opponent Team: ", newdata()$Opp,
                                 "<br>Minutes Played: ", newdata()$MP,
                                 "<br>Steals: ", newdata()$STL,
                                 "<br>Field Goal Attempts: ", newdata()$FGA,
                                 "<br>Wins: ", newdata()$WL),
                    hoverinfo = 'text')
     fig <- fig %>% layout(scene = list(xaxis = list(title = 'Minutes Played'),
                                        yaxis = list(title = 'Steals'),
                                        zaxis = list(title = 'Field Goal Attempts')))
     
     fig
   })
   
   post.results <- eventReactive(input$go, {
       values <- data.frame(MP=input$mp, STL=input$stl, TOV=input$fga,Opp=input$opp)
   })
   
   # Show the values in an HTML table ----
   output$values <- renderTable({
      pts.model <- lm(PTS ~ MP + STL + TOV + Opp,data=newdata())
      predict.pts <- predict(pts.model, newdata=post.results(), interval='predict')[1]
      data.frame(
         Estimate = c("Predicted Points:"),
         Value = c(predict.pts),
         stringsAsFactors = FALSE)
   })
   
   observe({
      updateSelectInput(session,"year1",
                        choices=unique(newdata()[,"Year"]))
   })
   
   output$plot4 <- renderPlot({
      playerstat <-  newdata() %>%
         select(MP, FG, ThreeP,TRB,AST,BLK,Year) %>%
         group_by(Year) %>% 
         summarise_all(funs(sum))
      
      playerstat_min <- playerstat %>%
         group_by(Year) %>%
         mutate( FG_min = FG / MP,
                 ThreeP_min = ThreeP / MP,
                 TRB_min = TRB / MP,
                 AST_min = AST / MP,
                 BLK_min = BLK / MP)
      playerstat_min <- playerstat_min[,-c(2:7)]
      
      z_score <- function(x){
         z = (x - mean(x)) / sd(x)
         return(z)
      }
      playerstat_min[,c(2:6)] <- sapply(playerstat_min[,c(2:6)], z_score)
      
      # convert into long format for plotting
      stat_long <- playerstat_min %>%
         melt(., id = "Year", measure.vars = c(2:6))
      t_score <- function(x){
         t = (x * 10) + 50
         t = ifelse(t > 100, 100, 
                    ifelse(t < 0, 0, t))
         return(t)
      }
     stat_long <- stat_long %>%
         mutate_at(vars(value), .funs = t_score)
      
     df1 <- subset(stat_long,Year==input$year1)
     
     plot4 <- ggplot(df1, aes(x = variable, y = value, fill = variable)) +
       geom_col(color = "white", width = 0.9) +
       coord_polar(theta = "x") +
       geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
       theme(axis.text.x = element_text(face = "bold", size = 12),
             legend.title = element_blank(),
             legend.position = "none",
             strip.text = element_text(face = "bold", size = 13)) +
       labs(x = "", y = "") +
       facet_wrap(~Year)+
       ylim(0, 100) +
       scale_fill_brewer(palette="Set2")
     
     print(plot4)
   })

   observe({
      updateSelectInput(session, "year2",
                        choices=unique(newdata()[,"Year"]))
   })
   output$plot5 <- renderPlot({
      playerstat <-  newdata() %>%
         select(MP, FG, ThreeP,TRB,AST,BLK,Year) %>%
         group_by(Year) %>% 
         summarise_all(funs(sum))
      
      playerstat_min <- playerstat %>%
         group_by(Year) %>%
         mutate( FG_min = FG / MP,
                 ThreeP_min = ThreeP / MP,
                 TRB_min = TRB / MP,
                 AST_min = AST / MP,
                 BLK_min = BLK / MP)
      playerstat_min <- playerstat_min[,-c(2:7)]
      
      z_score <- function(x){
         z = (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
         return(z)
      }
      playerstat_min[,c(2:6)] <- sapply(playerstat_min[,c(2:6)], z_score)
      
      # convert into long format for plotting
      stat_long <- playerstat_min %>%
         melt(., id = "Year", measure.vars = c(2:6))
      t_score <- function(x){
         t = (x * 10) + 50
         t = ifelse(t > 100, 100, 
                    ifelse(t < 0, 0, t))
         return(t)
      }
      stat_long <- stat_long %>%
         mutate_at(vars(value), .funs = t_score)

     df1 <- subset(stat_long,Year==input$year2)
     
     plot5 <- ggplot(df1, aes(x = variable, y = value, fill = variable)) +
       geom_col(color = "white", width = 0.9) +
       coord_polar(theta = "x") +
       geom_hline(yintercept = seq(50, 50, by = 1), size = 1.2) +
       theme(axis.text.x = element_text(face = "bold", size = 12),
             legend.title = element_blank(),
             legend.position = "none",
             strip.text = element_text(face = "bold",size = 13)) +
       labs(x = "", y = "") +
       facet_wrap(~Year)+
       ylim(0, 100) +
       scale_fill_brewer(palette="Set2")
     
     print(plot5)
   })
   
   ## create player stats tables
   output$stat.table <- renderTable({
     playerstat <-  newdata() %>%
         select(MP, FG, ThreeP,TRB,AST,BLK,Year) %>%
         group_by(Year) %>% 
         summarise_all(funs(sum))
     df2 <- subset(playerstat, Year %in% c(input$year1, input$year2))
   })
    
   observe({
      updateSelectInput(session, "opp",
                        choices=unique(newdata()[,"Opp"]))
   })
   
}

shinyApp(ui=ui,server = server)

