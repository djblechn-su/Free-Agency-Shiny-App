# Libraries to download

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(DT)
library(tidyverse)
library(tidyr)
library(htmltools)
library(dplyr)
library(rvest)
library(XML)
library(httr)
library(stringr)
library(magrittr)
library(gtools)
library(tibble)

setwd("/Users/dugbl/OneDrive/Documents/R STUFF/NBA 2018/RDS")

AllConf     <- readRDS("AllConf.rds"    )
ChooseTeam  <- readRDS("ChooseTeam.rds" )
FreeAgents  <- readRDS("FreeAgents.rds" )
Page1FAs    <- readRDS("Page1FAs.rds"   )
Signed      <- readRDS("Signed.rds"     )
Totals      <- readRDS("Totals.rds"     )
TotalsPage1 <- readRDS("TotalsPage1.rds")

# Now Start
ui <- fluidPage(
# Use JavaScript 
  useShinyjs(), 
# Change Theme  
  theme = shinytheme("superhero"),
                tags$style(HTML(
                  "@import url('https://fonts.googleapis.com/css?family=Aldrich')")),
# Change DT marks to orange
                fluidPage(
                  tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                            background-color: #EF911B !important;
                                            }
                                            "))),
                  tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
                                  color: #EF911B !important;
                                  }")),
# Change Font of Totals DTs to 0px                  
                  tags$head(tags$style(type = "text/css", "#Totals1 th {font-size:0px;}")),
                  tags$head(tags$style(type = "text/css", "#Totals2 th {font-size:0px;}")),
                  tags$head(tags$style(type = "text/css", "#NegTeam th {font-size:0px;}")),
                  tags$head(tags$style(type = "text/css", "#NegTotals th {font-size:0px;}")),
# Title of Page                 
      titlePanel(div("NBA Offseason Simulation", style = "color: #EF911B")),
# Start Tabs              
        tabsetPanel(type = "pills", id = "inTabset",
#Choose Team Tab                    
                 tabPanel("Choose Team",
              verticalLayout(
                fluidRow(
                  column(6, selectInput("Team", 
                                         "Select Team", 
                                         choices = sort(Signed$Tm), 
                                         selected=T)),
                  column(6, actionButton("chooseteam", "Select"))),
                 fluidRow(
                   headerPanel(
                     h3("Signed Players",
                         style = "font-family: Times New Roman, serif;
                         font-weight: 400;
                         line-height: 1;
                         color: #EF911B"))),
                 fluidRow(
                   column(12, (
                     div(DT::dataTableOutput("Tab"),
                         style = "font-family: Arial, Helvetica, sans-serif;
                         font-weight: 700;
                         line-height: 1.6;
                         color: #000000")))),
                 fluidRow(
                   column(12, (
                     div(DT::dataTableOutput("Totals1"),
                         style = "font-family: Arial, Helvetica, sans-serif;
                         font-weight: 700;
                         line-height: 1.6;
                         color: #000000")))),
                 fluidRow(
                   headerPanel(
                     h3("Free Agents",
                        style = "font-family: Times New Roman, serif;
                        font-weight: 400;
                        line-height: 1;
                        color: #EF911B"))),
                 fluidRow(
                   column(12, (
                     div(DT::dataTableOutput("P1FAs"),
                         style = "font-family: Arial, Helvetica, sans-serif;
                         font-weight: 700;
                         line-height: 1.6;
                         color: #000000")))))),
# FAs Panel
                  tabPanel("FAs",
              verticalLayout(
                fluidRow(
                 column(6, style='padding:0px;', 
                  headerPanel(
                    h2("Signed Players",
                        style = "font-family: Times New Roman, serif;
                        font-weight: 400;
                        line-height: 1;
                        color: #EF911B"))), 
                   column(6, style='padding:0px;', 
                    headerPanel(
                     h2("Free Agents",
                         style = "font-family: Times New Roman, serif;
                         font-weight: 400;
                         line-height: 1;
                         color: #EF911B")))),
                fluidRow(
                  column(6, (div(
                    DT::dataTableOutput("Tab2"),
                      style = "font-family: Arial, Helvetica, sans-serif;
                      font-weight: 700;
                      line-height: 1.6;
                      color: #000000"))),
                  column(6, (div(
                    DT::dataTableOutput("FA"),
                      style = "font-family: Arial, Helvetica, sans-serif;
                      font-weight: 700;
                      line-height: 1.6;
                      color: #000000"))))),
              fluidRow(
                column(6, (
                  div(DT::dataTableOutput("Totals2"),
                    style = "font-family: Arial, Helvetica, sans-serif;
                    font-weight: 700;
                    line-height: 1.6;
                    color: #000000")))),
                fluidRow(
                  column(6, (
                    actionButton("reset2", "Reset"))),
                  column(6, (
                    actionButton("addrow2", "Sign"))))),
# # Trade Panel
#               tabPanel("Trade",
#                 verticalLayout(
#                   fluidRow(
#                     column(6, style='padding:0px;', headerPanel(
#                       h2("Signed Players",
#                         style = "font-family: Times New Roman, serif;
#                         font-weight: 400;
#                         line-height: 1;
#                         color: #EF911B"))), 
#                     column(6, style='padding:0px;', headerPanel(
#                       h2("Trade",
#                          style = "font-family: Times New Roman, serif;
#                          font-weight: 400;
#                          line-height: 1;
#                          color: #EF911B")))),
#                   fluidRow(
#                     column(6, offset = 6, selectInput("Team2", 
#                                           "Filter By Team", 
#                                           choices = sort(Signed$Tm), 
#                                           selected=T))),
#                   fluidRow(
#                     column(6, (div(
#                       DT::dataTableOutput("Tab3"),
#                       style = "font-family: Arial, Helvetica, sans-serif;
#                       font-weight: 700;
#                       line-height: 1.6;
#                       color: #000000"))),
#                     column(6, (div(
#                       DT::dataTableOutput("Tab4"),
#                       style = "font-family: Arial, Helvetica, sans-serif;
#                       font-weight: 700;
#                       line-height: 1.6;
#                       color: #000000")))))),
# Negotiate Tab
                tabPanel("Negotiate",
                  verticalLayout(
                    fluidRow(
                      column(3, selectInput("PType", 
                                          "Player Type", 
                                          choices = c("Roster", "Two-Way"), 
                                          selected = T)),
                      column(3, selectInput("YrsLeft",
                                          "Select Contract Length",
                                          choices = c(1,2,3,4,5),
                                          selected = T)),
                      column(3, numericInput("ContractNum",
                                        "Select Per Year Salary",
                                        min = 838464,
                                        max = 35654150,
                                        value = 838464,
                                        step = 1,
                                        width = '100%'))),
                    fluidRow(
                      column(12, 
                        sliderInput("Contract",
                                    "Select Per Year Salary",
                                    min = 838464,
                                    max = 35654150,
                                    value = 838464,
                                    step = 1,
                                    width = '100%'))),
                    fluidRow(
                      column(3,
                        actionButton("set", "Change Value to Predicted Sal")),
                      column(3,
                        actionButton("set3", "Change Value to Max Sal")),
                      column(6,
                        actionButton("set2", "Offer Salary"))),
                    fluidRow(
                      column(12, 
                             span(textOutput("txt"), 
                                  style="font-family: Times New Roman, serif;
                                  font-size: 100px;
                                  font-weight: 400;
                                  line-height: 1;
                                  color: #EF911B"))),
                    fluidRow(
                      column(12, (div(
                        DT::dataTableOutput("tmp"),
                        style = "font-family: Arial, Helvetica, sans-serif;
                        font-weight: 700;
                        line-height: 1.6;
                        color: #000000")))),
                    fluidRow(
                      column(6,
                        actionButton("resetToFAs", "Back to FAs"))),
                    fluidRow(
                      column(12, (div(
                        DT::dataTableOutput("NegTeam"),
                        style = "font-family: Arial, Helvetica, sans-serif;
                        font-weight: 700;
                        line-height: 1.6;
                        color: #000000")))),
                    fluidRow(
                      column(12, (div(
                        DT::dataTableOutput("NegTotals"),
                        style = "font-family: Arial, Helvetica, sans-serif;
                        font-weight: 700;
                        line-height: 1.6;
                        color: #000000")))),
                    fluidRow(
                      column(1, offset = 11,
                        actionButton("continueToFAs", "Continue"))))))))

###############################################################################################################################

# SERVER

server <- function(input, output, session) {
  
  hideElement(id="continueToFAs")
  
  hideTab(inputId = "inTabset",
          target = "Negotiate")
  
  colnames(ChooseTeam) = c('<span style="color:#EF911B">Player</span>',
                       'Tm',
                       '<span style="color:#EF911B">Pos</span>',
                       '<span style="color:#EF911B">Age</span>',
                       '<span style="color:#EF911B">Exp</span>',
                       '<span style="color:#EF911B">G</span>',
                       '<span style="color:#EF911B">PPG</span>',
                       '<span style="color:#EF911B">RPG</span>',
                       '<span style="color:#EF911B">APG</span>',
                       '<span style="color:#EF911B">FG%</span>',
                       '<span style="color:#EF911B">3P%</span>',
                       '<span style="color:#EF911B">FT%</span>',
                       '<span style="color:#EF911B">eFG%</span>',
                       '<span style="color:#EF911B">PER</span>',
                       '<span style="color:#EF911B">Years Left</span>',
                       '<span style="color:#EF911B">2018-19 Salary</span>',
                       '<span style="color:#EF911B">Cap Pct</span>',
                       '<span style="color:#EF911B">Lux Pct</span>',
                       '<span style="color:#EF911B">Player Type</span>')
  
  output$Tab <- DT::renderDataTable({
    DT::datatable(ChooseTeam %>%
                    filter(Tm==input$Team) %>%
                  select('<span style="color:#EF911B">Player</span>',
                         '<span style="color:#EF911B">Pos</span>',
                         '<span style="color:#EF911B">Age</span>',
                         '<span style="color:#EF911B">Exp</span>',
                         '<span style="color:#EF911B">G</span>',
                         '<span style="color:#EF911B">PPG</span>',
                         '<span style="color:#EF911B">RPG</span>',
                         '<span style="color:#EF911B">APG</span>',
                         '<span style="color:#EF911B">FG%</span>',
                         '<span style="color:#EF911B">3P%</span>',
                         '<span style="color:#EF911B">FT%</span>',
                         '<span style="color:#EF911B">eFG%</span>',
                         '<span style="color:#EF911B">PER</span>',
                         '<span style="color:#EF911B">Years Left</span>',
                         '<span style="color:#EF911B">2018-19 Salary</span>',
                         '<span style="color:#EF911B">Cap Pct</span>',
                         '<span style="color:#EF911B">Lux Pct</span>',
                         '<span style="color:#EF911B">Player Type</span>'),
        options = list(paging = FALSE, scrollX = F, searching = FALSE, bInfo = FALSE,
                       autoWidth = F, 
                       columnDefs = list(list(width = '250px', targets = c(0)),
                                         list(width = '100px', targets = c(14)),
                                         list(width = '40px', targets = c(1:13,17)))),
        class = 'cell-border hover compact',
        rownames = FALSE,
        escape = FALSE) %>%
      formatCurrency(columns = '<span style="color:#EF911B">2018-19 Salary</span>',
                     currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c('<span style="color:#EF911B">Cap Pct</span>',
                              '<span style="color:#EF911B">Lux Pct</span>'),
                              digits = 2)})
  
  output$Totals1 <- DT::renderDataTable({
    DT::datatable(TotalsPage1 %>%
                    filter(Tm==input$Team) %>%
                    select('Player', 'Pos', 'Age', 'Exp', 'G', 'PPG', 'RPG', 'APG', 'FG%', '3P%', 'FT%', 'eFG%', 'PER', 'Years Left', '2018-19 Salary', 'Cap Pct', 'Lux Pct', 'Player Type'),
                  options = list(paging = FALSE, scrollX = F, searching = FALSE, bInfo = FALSE, ordering = F,
                                 autoWidth = F, 
                                 columnDefs = list(list(width = '250px', targets = c(0)),
                                                   list(width = '100px', targets = c(14)),
                                                   list(width = '40px', targets = c(1:13,17)))),
                  class = 'cell-border hover compact',
                  rownames = FALSE)%>%
      formatCurrency(columns = '2018-19 Salary',
                     currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c('Age', 'Exp', 'G'),
                  digits = 0) %>%
      formatRound(columns = c('PPG', 'RPG', 'APG'),
                  digits = 1) %>%
      formatRound(columns = c('Cap Pct', 'Lux Pct'),
                  digits = 2) %>%
      formatRound(columns = c('FG%', '3P%', 'FT%', 'eFG%'),
                  digits = 1)})
  
  colnames(Page1FAs) = c('<span style="color:#EF911B">Player</span>',
                           '<span style="color:#EF911B">FA Type</span>',
                         '<span style="color:#EF911B">FA Type 2</span>',
                           '<span style="color:#EF911B">Pos</span>',
                           'Tm',
                           '<span style="color:#EF911B">Age</span>',
                         '<span style="color:#EF911B">Exp</span>',
                         '<span style="color:#EF911B">G</span>',
                         '<span style="color:#EF911B">PPG</span>',
                         '<span style="color:#EF911B">RPG</span>',
                         '<span style="color:#EF911B">APG</span>',
                         '<span style="color:#EF911B">FG%</span>',
                         '<span style="color:#EF911B">3P%</span>',
                         '<span style="color:#EF911B">FT%</span>',
                         '<span style="color:#EF911B">eFG%</span>',
                           '<span style="color:#EF911B">PER</span>',
                           '<span style="color:#EF911B">2017-18 Salary</span>')
  
  output$P1FAs <- DT::renderDataTable({
    datatable(Page1FAs %>%
                filter(Tm==input$Team) %>%
                select('<span style="color:#EF911B">Player</span>',
                       '<span style="color:#EF911B">FA Type</span>',
                       '<span style="color:#EF911B">FA Type 2</span>',
                       '<span style="color:#EF911B">Pos</span>',
                       '<span style="color:#EF911B">Age</span>',
                       '<span style="color:#EF911B">Exp</span>',
                       '<span style="color:#EF911B">G</span>',
                       '<span style="color:#EF911B">PPG</span>',
                       '<span style="color:#EF911B">RPG</span>',
                       '<span style="color:#EF911B">APG</span>',
                       '<span style="color:#EF911B">FG%</span>',
                       '<span style="color:#EF911B">3P%</span>',
                       '<span style="color:#EF911B">FT%</span>',
                       '<span style="color:#EF911B">eFG%</span>',
                       '<span style="color:#EF911B">PER</span>',
                       '<span style="color:#EF911B">2017-18 Salary</span>'),
              options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE),
              class = 'cell-border hover compact',
              rownames = FALSE,
              escape = F) %>%
      formatCurrency(columns = c('<span style="color:#EF911B">2017-18 Salary</span>'), 
                     currency = "$", interval = 3, mark = ",", digits = 0)})
  
  #### Second Page
  
  observeEvent(input$chooseteam, {
      colnames(Signed) = c('<span style="color:#EF911B">Player</span>',
                           'Tm',
                           '<span style="color:#EF911B">Pos</span>',
                           '<span style="color:#EF911B">Age</span>',
                           '<span style="color:#EF911B">Exp</span>',
                           '<span style="color:#EF911B">PER</span>',
                           '<span style="color:#EF911B">Years Left</span>',
                           '<span style="color:#EF911B">2018-19 Salary</span>',
                           '<span style="color:#EF911B">Cap Pct</span>',
                           '<span style="color:#EF911B">Lux Pct</span>',
                           '<span style="color:#EF911B">Player Type</span>')
  
  output$Tab2 <- DT::renderDataTable({
    DT::datatable(Signed %>%
                    filter(Tm==input$Team) %>%
                    select('<span style="color:#EF911B">Player</span>',
                           '<span style="color:#EF911B">Pos</span>',
                           '<span style="color:#EF911B">Age</span>',
                           '<span style="color:#EF911B">Exp</span>',
                           '<span style="color:#EF911B">PER</span>',
                           '<span style="color:#EF911B">Years Left</span>',
                           '<span style="color:#EF911B">2018-19 Salary</span>',
                           '<span style="color:#EF911B">Cap Pct</span>',
                           '<span style="color:#EF911B">Lux Pct</span>',
                           '<span style="color:#EF911B">Player Type</span>'),
                  options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE,
                                 autoWidth = F, 
                                 columnDefs = list(list(width = '120px', targets = c(0)),
                                                   list(width = '35px', targets = c(1:9)))),
                  class = 'cell-border hover compact',
                  rownames = FALSE,
                  escape = FALSE) %>%
      formatCurrency(columns = '<span style="color:#EF911B">2018-19 Salary</span>',
                     currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c('<span style="color:#EF911B">Cap Pct</span>',
                              '<span style="color:#EF911B">Lux Pct</span>'),
                  digits = 2)})
  
  output$Totals2 <- DT::renderDataTable({
    DT::datatable(TotalsPage1 %>%
                    filter(Tm==input$Team) %>%
                    select('Player', 'Pos', 'Age', 'Exp', 'PER', 'Years Left', '2018-19 Salary', 'Cap Pct', 'Lux Pct', 'Player Type'),
                  options = list(paging = FALSE, scrollX = F, searching = FALSE, bInfo = FALSE, ordering = F,
                                 autoWidth = F, 
                                 columnDefs = list(list(width = '120px', targets = c(0)),
                                                   list(width = '35px', targets = c(1:9)))),
                  class = 'cell-border hover compact',
                  rownames = FALSE)%>%
      formatCurrency(columns = '2018-19 Salary',
                     currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c('Age', 'Exp'),
                  digits = 0) %>%
      formatRound(columns = c('Cap Pct', 'Lux Pct'),
                  digits = 2)})
  
  colnames(FreeAgents) = c('<span style="color:#EF911B">Player</span>',
                           '<span style="color:#EF911B">FA Type</span>',
                           '<span style="color:#EF911B">Pos</span>',
                           '<span style="color:#EF911B">Prev Tm</span>',
                           '<span style="color:#EF911B">Age</span>',
                           '<span style="color:#EF911B">Exp</span>',
                           '<span style="color:#EF911B">PER</span>',
                           '<span style="color:#EF911B">Pred 2018-19 Salary</span>',
                           '<span style="color:#EF911B">2017-18 Salary</span>',
                           '<span style="color:#EF911B">Cap Pct</span>',
                           '<span style="color:#EF911B">Lux Pct</span>')
  
  output$FA <- DT::renderDataTable({
    datatable(FreeAgents %>%
                select('<span style="color:#EF911B">Player</span>',
                       '<span style="color:#EF911B">FA Type</span>',
                       '<span style="color:#EF911B">Pos</span>',
                       '<span style="color:#EF911B">Prev Tm</span>',
                       '<span style="color:#EF911B">Age</span>',
                       '<span style="color:#EF911B">Exp</span>',
                       '<span style="color:#EF911B">PER</span>',
                       '<span style="color:#EF911B">Pred 2018-19 Salary</span>',
                       '<span style="color:#EF911B">2017-18 Salary</span>'),
              options = list(scrollX = FALSE, bInfo = FALSE),
              class = 'cell-border hover compact',
              selection = "single",
              rownames = FALSE,
              escape = F) %>%
      formatCurrency(columns = c('<span style="color:#EF911B">Pred 2018-19 Salary</span>', '<span style="color:#EF911B">2017-18 Salary</span>'), 
                     currency = "$", interval = 3, mark = ",", digits = 0)})
  
# Trade Page
  
  output$Tab3 <- DT::renderDataTable({
    DT::datatable(Signed %>%
                    filter(Tm == input$Team) %>%
                    select('<span style="color:#EF911B">Player</span>',
                           '<span style="color:#EF911B">Pos</span>',
                           '<span style="color:#EF911B">Age</span>',
                           '<span style="color:#EF911B">Exp</span>',
                           '<span style="color:#EF911B">PER</span>',
                           '<span style="color:#EF911B">Years Left</span>',
                           '<span style="color:#EF911B">2018-19 Salary</span>',
                           '<span style="color:#EF911B">Cap Pct</span>',
                           '<span style="color:#EF911B">Lux Pct</span>',
                           '<span style="color:#EF911B">Player Type</span>'),
                  options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE),
                  class = 'cell-border hover compact',
                  rownames = FALSE,
                  escape = FALSE) %>%
      formatCurrency(columns = '<span style="color:#EF911B">2018-19 Salary</span>',
                     currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c('<span style="color:#EF911B">Cap Pct</span>',
                              '<span style="color:#EF911B">Lux Pct</span>'),
                  digits = 2)})
  
  output$Tab4 <- DT::renderDataTable({
    DT::datatable(Signed %>%
                    filter(Tm==input$Team2) %>%
                    select('<span style="color:#EF911B">Player</span>',
                           '<span style="color:#EF911B">Pos</span>',
                           '<span style="color:#EF911B">Age</span>',
                           '<span style="color:#EF911B">Exp</span>',
                           '<span style="color:#EF911B">PER</span>',
                           '<span style="color:#EF911B">Years Left</span>',
                           '<span style="color:#EF911B">2018-19 Salary</span>',
                           '<span style="color:#EF911B">Cap Pct</span>',
                           '<span style="color:#EF911B">Lux Pct</span>',
                           '<span style="color:#EF911B">Player Type</span>'),
                  options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE),
                  class = 'cell-border hover compact',
                  rownames = FALSE,
                  escape = FALSE) %>%
      formatCurrency(columns = '<span style="color:#EF911B">2018-19 Salary</span>',
                     currency = "$", interval = 3, mark = ",", digits = 0) %>%
      formatRound(columns = c('<span style="color:#EF911B">Cap Pct</span>',
                              '<span style="color:#EF911B">Lux Pct</span>'),
                  digits = 2)})
  
  #### Negotiation Page
  
  updateTabsetPanel(session, "inTabset",
                    selected = "FAs")
  
  hideTab(inputId = "inTabset",
          target = "Choose Team")
  
    })
  
  observeEvent(input$reset2, {
    showTab(inputId = "inTabset", 
            target = "Choose Team")
    updateTabsetPanel(session, "inTabset",
                      selected = "Choose Team")
  })
  
  selectedRow <- eventReactive(input$FA_rows_selected,{
    row.names(FreeAgents)[c(input$FA_rows_selected)]
  })
  
  colnames(FreeAgents) = c('<span style="color:#EF911B">Player</span>',
                     '<span style="color:#EF911B">FA Type</span>',
                     '<span style="color:#EF911B">Pos</span>',
                     '<span style="color:#EF911B">Prev Tm</span>',
                     '<span style="color:#EF911B">Age</span>',
                     '<span style="color:#EF911B">Exp</span>',
                     '<span style="color:#EF911B">PER</span>',
                     '<span style="color:#EF911B">Pred 2018-19 Salary</span>',
                     '<span style="color:#EF911B">2017-18 Salary</span>',
                     '<span style="color:#EF911B">Cap Pct</span>',
                     '<span style="color:#EF911B">Lux Pct</span>')

  NegotiateTab <- reactive({
    FreeAgents %>%
      filter(row.names(FreeAgents)==selectedRow()) %>%
      select('<span style="color:#EF911B">Player</span>',
             '<span style="color:#EF911B">FA Type</span>',
             '<span style="color:#EF911B">Pos</span>',
             '<span style="color:#EF911B">Age</span>',
             '<span style="color:#EF911B">Exp</span>',
             '<span style="color:#EF911B">PER</span>',
             '<span style="color:#EF911B">Pred 2018-19 Salary</span>')
  })
  
  observeEvent(input$addrow2, {
    showTab(inputId = "inTabset", 
            target = "Negotiate")
    hideTab(inputId = "inTabset", 
            target = "FAs")
    hideTab(inputId = "inTabset", 
            target = "Trade")
    updateTabsetPanel(session, "inTabset",
                      selected = "Negotiate")
    hideElement(id="continueToFAs")
    hideElement(id="txt")
    
    output$tmp <- DT::renderDataTable({
      DT::datatable(NegotiateTab() %>%
                      select('<span style="color:#EF911B">Player</span>',
                             '<span style="color:#EF911B">Pos</span>',
                             '<span style="color:#EF911B">Age</span>',
                             '<span style="color:#EF911B">Exp</span>',
                             '<span style="color:#EF911B">PER</span>'),
               options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE, ordering = FALSE),
               class = 'cell-border hover compact',
               rownames = FALSE,
               escape = F)})
    
    updateNumericInput(session,
                      "ContractNum",
                      value =
                        if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==0) {838464}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==1)	{1349383}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==2)	{1512601}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==3) {1567007}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==4)	{1621415}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==5) {1757429}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==6)	{1893447}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {2029463}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8)	{2165481}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {2176260}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {2393887},
                      min =
                        if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==0) {838464}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==1)	{1349383}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==2)	{1512601}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==3) {1567007}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==4)	{1621415}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==5) {1757429}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==6)	{1893447}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {2029463}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8)	{2165481}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {2176260}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {2393887},
                      max =
                        if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'<=6) {25467250}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {30560700}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8) {30560700}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {30560700}
                      else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {35654150})
    
    
    
      updateSliderInput(session,
                        "Contract",
                        value =
                          if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==0) {838464}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==1)	{1349383}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==2)	{1512601}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==3) {1567007}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==4)	{1621415}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==5) {1757429}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==6)	{1893447}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {2029463}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8)	{2165481}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {2176260}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {2393887},
                        min =
                          if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==0) {838464}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==1)	{1349383}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==2)	{1512601}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==3) {1567007}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==4)	{1621415}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==5) {1757429}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==6)	{1893447}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {2029463}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8)	{2165481}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {2176260}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {2393887},
                        max =
                          if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'<=6) {25467250}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {30560700}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8) {30560700}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {30560700}
                        else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {35654150})
      })
  
  observe({
    updateSliderInput(session,
                      "Contract",
                      value = input$ContractNum)
  })
  
  observe({
    updateNumericInput(session,
                      "ContractNum",
                      value = input$Contract)
  })
  
  observe({
    if(input$PType=="Roster"){
      showElement("Contract")
      showElement("ContractNum")
      showElement("set")
      showElement("set3")
      updateSelectInput(session, "YrsLeft", choices = c(1,2,3,4,5))
    }
  })
  
  observe({
    if(input$PType=="Two-Way"){
      hideElement("Contract")
      hideElement("ContractNum")
      hideElement("set")
      hideElement("set3")
      updateSelectInput(session, "YrsLeft", choices = c(1,2))
      updateSliderInput(session, "Contract", value = 0)
      updateNumericInput(session, "ContractNum", value = 0)
    }
  })
  
  observeEvent(input$set, {
    sal = NegotiateTab()$'<span style="color:#EF911B">Pred 2018-19 Salary</span>'
    updateSliderInput(session, "Contract", value = sal)
    updateNumericInput(session, "ContractNum", value = sal)
  })
  
  observeEvent(input$set3, {
    sal = if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'<=6) {25467250}
    else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==7) {30560700}
    else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==8) {30560700}
    else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'==9) {30560700}
    else if(NegotiateTab()$'<span style="color:#EF911B">Exp</span>'>=10) {35654150}
    updateSliderInput(session, "Contract", value = sal)
    updateNumericInput(session, "ContractNum", value = sal)
  })
  
  sliderVal <- reactive({
    if(input$PType=="Roster"){
      as.numeric(input$Contract)
    } else {NA}
  })
  yrsVal <- reactive({as.numeric(input$YrsLeft)})
  PlayerTypeVal <- reactive({input$PType})
  
  colnames(AllConf)[51] <- 50
  
  Pcts <- reactive({
    AllConf %>%
      filter(row.names(AllConf)==selectedRow())
  })
  
  Pcts1 <- reactive({
    as.data.frame(t(Pcts()))
  })
  
  Pcts2 <- reactive({
    Pcts1() -> tmp
    colnames(tmp) <- "Player"
    tmp$Per <- c(0:100)
    tmp <- tmp[,c(2,1)]
    tmp %>% arrange(desc(Per)) -> tmp
    tmp$Player[duplicated(tmp$Player)] <- NA
    tmp %>% arrange(Per) -> tmp
    tmp
  })
  
  Likelies <- reactive({
    ifelse(Pcts2()$Player==sliderVal(),
           which(Pcts2()$Player==sliderVal())-1,
           which.min(abs(sliderVal() - Pcts2()$Player))-1)
  })
  

  Likelies1 <- reactive({
    as.numeric(Likelies())
  })
  
  Likelies2 <- reactive({
    ifelse(is.na(Likelies()[1]), 100, Likelies1())
  })
  
  CapMax <- 101800000
  LuxTax <- 123000000
  
  CapPct <- reactive({
    if(input$PType=="Roster"){
      (sliderVal()/CapMax)*100
    } else {NA}
  })
  
  LuxPct <- reactive({
    if(input$PType=="Roster"){
      (sliderVal()/LuxTax)*100
    } else {NA}
  })
  
  NegotiateTab2 <- reactive({cbind(NegotiateTab(), CapPct(), LuxPct(), sliderVal(), yrsVal(), PlayerTypeVal())})
  
  NegotiateTab3 <- reactive({
    NegotiateTab2() -> tmp
    colnames(tmp) <- c('<span style="color:#EF911B">Player</span>',
                       '<span style="color:#EF911B">FA Type</span>',
                       '<span style="color:#EF911B">Pos</span>',
                       '<span style="color:#EF911B">Age</span>',
                       '<span style="color:#EF911B">Exp</span>',
                       '<span style="color:#EF911B">PER</span>',
                       '<span style="color:#EF911B">Pred 2018-19 Salary</span>', 
                       '<span style="color:#EF911B">Cap Pct</span>',
                       '<span style="color:#EF911B">Lux Pct</span>',
                       '<span style="color:#EF911B">2018-19 Salary</span>',
                       '<span style="color:#EF911B">Years Left</span>',
                       '<span style="color:#EF911B">Player Type</span>')
    tmp
  })
  
  RandNum <- as.numeric(sample(0:100, 1))
  
    observeEvent(input$set2, {
      if(RandNum <= Likelies2()[1]) {
    output$tmp <- DT::renderDataTable({
      DT::datatable(NegotiateTab3() %>%
                      select('<span style="color:#EF911B">Player</span>',
                             '<span style="color:#EF911B">Pos</span>',
                             '<span style="color:#EF911B">Age</span>',
                             '<span style="color:#EF911B">Exp</span>',
                             '<span style="color:#EF911B">PER</span>',
                             '<span style="color:#EF911B">Years Left</span>',
                             '<span style="color:#EF911B">2018-19 Salary</span>', 
                             '<span style="color:#EF911B">Cap Pct</span>',
                             '<span style="color:#EF911B">Lux Pct</span>',
                             '<span style="color:#EF911B">Player Type</span>'),
                    options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE, ordering = FALSE,
                                   autoWidth = F, 
                                   columnDefs = list(list(width = '120px', targets = c(0,6)),
                                                     list(width = '40px', targets = c(1:5,7:9)))),
                    class = 'cell-border hover compact',
                    rownames = FALSE,
                    escape = F) %>%
        formatCurrency(columns = '<span style="color:#EF911B">2018-19 Salary</span>',
                       currency = "$", interval = 3, mark = ",", digits = 0) %>%
        formatRound(columns = c('<span style="color:#EF911B">Cap Pct</span>',
                                '<span style="color:#EF911B">Lux Pct</span>'),
                    digits = 2)})
        output$txt <- renderText({"Player Signed"})
        colnames(Signed) = c('<span style="color:#EF911B">Player</span>',
                             'Tm',
                             '<span style="color:#EF911B">Pos</span>',
                             '<span style="color:#EF911B">Age</span>',
                             '<span style="color:#EF911B">Exp</span>',
                             '<span style="color:#EF911B">PER</span>',
                             '<span style="color:#EF911B">Years Left</span>',
                             '<span style="color:#EF911B">2018-19 Salary</span>',
                             '<span style="color:#EF911B">Cap Pct</span>',
                             '<span style="color:#EF911B">Lux Pct</span>',
                             '<span style="color:#EF911B">Player Type</span>')
        
        output$NegTeam <- DT::renderDataTable({
          DT::datatable(Signed %>%
                          filter(Tm==input$Team) %>%
                          select('<span style="color:#EF911B">Player</span>',
                                 '<span style="color:#EF911B">Pos</span>',
                                 '<span style="color:#EF911B">Age</span>',
                                 '<span style="color:#EF911B">Exp</span>',
                                 '<span style="color:#EF911B">PER</span>',
                                 '<span style="color:#EF911B">Years Left</span>',
                                 '<span style="color:#EF911B">2018-19 Salary</span>',
                                 '<span style="color:#EF911B">Cap Pct</span>',
                                 '<span style="color:#EF911B">Lux Pct</span>',
                                 '<span style="color:#EF911B">Player Type</span>'),
                        options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE, ordering = FALSE,
                                       autoWidth = F, 
                                       columnDefs = list(list(width = '120px', targets = c(0,6)),
                                                         list(width = '40px', targets = c(1:5,7:9)))),
                        class = 'cell-border hover compact',
                        rownames = FALSE,
                        escape = FALSE) %>%
            formatCurrency(columns = '<span style="color:#EF911B">2018-19 Salary</span>',
                           currency = "$", interval = 3, mark = ",", digits = 0) %>%
            formatRound(columns = c('<span style="color:#EF911B">Cap Pct</span>',
                                    '<span style="color:#EF911B">Lux Pct</span>'),
                        digits = 2)})
        # 
        # SalTot <- reactive({
        #   Signed %>%
        #     filter(Tm==input$Team) %>%
        #     select('<span style="color:#EF911B">2018-19 Salary</span>') %>%
        #     replace(is.na(.), 0) %>%
        #     summarise_all(funs(sum))
        # })
        # 
        # CapPctTot <- reactive({
        #   Signed %>%
        #     filter(Tm==input$Team) %>%
        #     select('<span style="color:#EF911B">Cap Pct</span>') %>%
        #   replace(is.na(.), 0) %>%
        #     summarise_all(funs(sum))
        # })
        # 
        # LuxPctTot <- reactive({
        #   Signed %>%
        #     filter(Tm==input$Team) %>%
        #     select('<span style="color:#EF911B">Lux Pct</span>') %>%
        #   replace(is.na(.), 0) %>%
        #     summarise_all(funs(sum))
        # })
        # 
        output$NegTotals <- DT::renderDataTable({
          DT::datatable(TotalsPage1 %>%
                          filter(Tm==input$Team) %>%
                          select('Player', 'Pos', 'Age', 'Exp', 'PER', 'Years Left', '2018-19 Salary', 'Cap Pct', 'Lux Pct', 'Player Type'),
                        options = list(paging = FALSE, scrollX = F, searching = FALSE, bInfo = FALSE, ordering = F,
                                       autoWidth = F, 
                                       columnDefs = list(list(width = '120px', targets = c(0,6)),
                                                         list(width = '40px', targets = c(1:5,7:9)))),
                        class = 'cell-border hover compact',
                        rownames = FALSE)%>%
            formatCurrency(columns = '2018-19 Salary',
                           currency = "$", interval = 3, mark = ",", digits = 0) %>%
            formatRound(columns = c('Age', 'Exp'),
                        digits = 0) %>%
            formatRound(columns = c('Cap Pct', 'Lux Pct'),
                        digits = 2)})
        
        showElement(id ="continueToFAs")
        showElement(id="txt")
        showElement(id ="NegTeam")
        showElement(id="NegTotals")
        
      } else if(RandNum > Likelies2()[1]) {
        output$tmp <- DT::renderDataTable({
          DT::datatable(NegotiateTab() %>%
                          select('<span style="color:#EF911B">Player</span>',
                                 '<span style="color:#EF911B">Pos</span>',
                                 '<span style="color:#EF911B">Age</span>',
                                 '<span style="color:#EF911B">Exp</span>',
                                 '<span style="color:#EF911B">PER</span>'),
                        options = list(paging = FALSE, scrollX = FALSE, searching = FALSE, bInfo = FALSE, ordering = FALSE),
                        class = 'cell-border hover compact',
                        rownames = FALSE,
                        escape = F)})
        output$txt <- renderText({"Negotiations Failed"})
        showElement(id="continueToFAs")
    }
      })
    
  observeEvent(input$set2, {
    showElement(id="txt")
    hideElement(id="PType")
    hideElement(id="YrsLeft")
    hideElement(id="ContractNum")
    hideElement(id="Contract")
    hideElement(id="set")
    hideElement(id="set2")
    hideElement(id="set3")
    hideElement(id="resetToFAs")
  })
  
  observeEvent(input$resetToFAs, {
    showTab(inputId = "inTabset", 
            target = "FAs")
    showTab(inputId = "inTabset", 
            target = "Trade")
    hideTab(inputId = "inTabset", 
            target = "Negotiate")
    updateTabsetPanel(session, "inTabset",
                      selected = "FAs")
    updateSelectInput(session, "PType", selected = "Roster")
    updateSelectInput(session, "YrsLeft", selected = 1)
  })
  
  selectedSigned <- eventReactive(input$tmp_rows_all, {
    input$tmp_rows_all
  })
  
  selectedSigned2 <- reactive({
    selectedSigned %>% select('<span style="color:#EF911B">Player</span>',
                              '<span style="color:#EF911B">Pos</span>',
                              '<span style="color:#EF911B">Age</span>',
                              '<span style="color:#EF911B">Exp</span>',
                              '<span style="color:#EF911B">PER</span>',
                              '<span style="color:#EF911B">Years Left</span>',
                              '<span style="color:#EF911B">2018-19 Salary</span>',
                              '<span style="color:#EF911B">Cap Pct</span>',
                              '<span style="color:#EF911B">Lux Pct</span>',
                              '<span style="color:#EF911B">Player Type</span>')
  })
  
  observeEvent(input$continueToFAs, {
    showTab(inputId = "inTabset", 
            target = "FAs")
    showTab(inputId = "inTabset", 
            target = "Trade")
    hideTab(inputId = "inTabset", 
            target = "Negotiate")
    updateTabsetPanel(session, "inTabset",
                      selected = "FAs")
    updateSelectInput(session, "PType", selected = "Roster")
    updateSelectInput(session, "YrsLeft", selected = 1)
    hideElement(id ="continueToFAs")
    hideElement(id="txt")
    hideElement(id ="NegTeam")
    hideElement(id="NegTotals")
    showElement(id="PType")
    showElement(id="YrsLeft")
    showElement(id="ContractNum")
    showElement(id="Contract")
    showElement(id="set")
    showElement(id="set2")
    showElement(id="set3")
    showElement(id="resetToFAs")
  })
  
}

shinyApp(ui, server)

