# R Shiny app for visual orgaanogram
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(visNetwork)
library(shinydashboard)
library(data.table)
library(tidyverse)
library(DT)
library(mailtoR)

#source("1_loadQaData.R")
source("loadData.R")
source("prepareData.R")

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Offshore Wind Process Map"),
  
  dashboardSidebar(disable = FALSE,
                   #width=600,
                   #br(), br(),
                   
                   selectInput("toCol", "choose `To`", 
                               c("To", "AlsoTo"),
                               selected = "To", multiple = FALSE),
                   
                   "Appearance options",
                   selectInput("groupCol", label = NULL, 
                               c("id")),
                   selectInput("colCol", label = NULL, 
                               c("color Category", "color Stage", "same Color"), 
                               selected = "color Profession", multiple = FALSE),
                   selectInput("shapeCol", label = NULL, 
                               c("shape Category", "shape Stage", "same Shape"), 
                               selected = "same shape", multiple = FALSE),
                   selectInput("sizeCol", label = NULL, 
                               c("size Category", "same Size"), 
                               selected = "size Category", multiple = FALSE),
                   
                   #"To filter the dataset. Multiple options can be selected",
                   selectInput("chooseCategory", "filter by Category", c(unique(nodeData$`Category`)), 
                               selected = c(unique(nodeData$`Category`)), multiple = TRUE),
                   selectInput("chooseStage", "filter by Stage", c(unique(nodeData$`Stage`)), 
                               selected = c(unique(nodeData$`Stage`)), multiple = TRUE)
  ),
  dashboardBody(
    
    #html tags to size boxes and netowrks
    tags$head(tags$style("
                    #boxfitscreen1{height:1000px !important;}
                     #real1{height:950px !important;}
                     #boxfitscreen2{height:1000px !important;}
                      #real2{height:950px !important;}
                     ")), 
    
    tabsetPanel(
      
      #added boxes to sort out sizing
      tabPanel("Offshore Wind Process Map",
               box(id = "boxfitscreen1", title = "This view displays the process map more traditionally",
                   visNetworkOutput("real1"), width = "100%", height = "auto")),
      
      
    )
  ),
  
  # Package to be able to send emails from R
  use_mailtoR()
  
)

# Define server logic ----
server <- shinyServer(function(input, output) {
  
  reactive_nodeData <- reactive({
    nodeData %>%
      select(c(`id`, 
               color = input$colCol,
               size = input$sizeCol,
               shape = input$shapeCol,
               `Category`,
               `Stage`,
               `To`,
               input$groupCol)) %>%
      filter(`Category` %in% input$chooseCategory) %>%
      filter(`Stage` %in% input$chooseStage)
  })
  
  reactive_edgeData <- reactive({ reactive_nodeData() %>% 
      select(from = input$groupCol,
             to = input$toCol)
  })
  
  
  #### Traditional visnetwork --------------------------------------------------
  
  output$real1 <- renderVisNetwork({
    visNetwork(reactive_nodeData(), reactive_edgeData()) %>%
      visNodes(physics = TRUE,
               mass = 5) %>% 
      # visHierarchicalLayout(direction="UD",
      #                       levelSeparation = 300,
      #                       nodeSpacing = 150,
      #                       ) %>%
      visOptions(highlightNearest=TRUE, 
                 nodesIdSelection = TRUE) %>%
      visOptions(selectedBy = input$groupCol) #%>%
    #allow for long click to select additional nodes
    #visInteraction(multiselect = FALSE) %>% #set to TRUE to be able to select more than one node
    #Use visEvents to turn set input$current_node_selection to list of selected nodes
    #visEvents(select = "function(nodes) {
    #Shiny.onInputChange('current_node_selection', nodes.nodes);
    #;}")
  })
  
  #### Alternative visnetwork --------------------------------------------------
  
  output$real2 <- renderVisNetwork({
    #visNetwork(nodeDataAlt, reactive_edgeDataAlt()) %>%
    visNetwork(reactive_nodeData(), reactive_edgeData()) %>%
      visIgraphLayout(layout = input$view,
                      physics = TRUE) %>%
      visOptions(highlightNearest=TRUE, 
                 nodesIdSelection = TRUE) %>%
      visOptions(selectedBy = input$groupCol) %>%
      #allow for long click to select additional nodes
      visInteraction(multiselect = FALSE) %>% #set to TRUE to be able to select more than one node
      #Use visEvents to turn set input$current_node_selection to list of selected nodes
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_selection', nodes.nodes);
                ;}")
  })
  
  #### Analysts Table ------------------------------------------------------------
  
  output$tabletofilter <- DT::renderDataTable({ 
    DT::datatable(nodeData %>% 
                    select(c(`id`, `Stage`, `Category`)), # %>%
                  #if want to filter from LHS
                  #filter(`G5 Division` %in% input$chooseDivision) %>%
                  #filter(`Grade (Organogram)` %in% input$chooseGrade),
                  
                  class = 'cell-border stripe',
                  filter = "top", 
                  rownames = F,
                  escape = FALSE,
                  options = list(
                    search = list(regex = TRUE),
                    autoWidth = FALSE, initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': 'white', 'color': 'black'});",
                      "}"),
                    pageLength = 50,
                    lengthMenu = list (c(25,50,100, -1), 
                                       c('25', '50', '100', 'All')), 
                    columnDefs = list(list(className = 'dt-left', targets = 0:4)),
                    dom = "ltBp",
                    buttons = list("copy", "csv")),
                  extensions = c("Buttons"),
                  selection = "single")
  })
  
  #### Summary Table -------------------------------------------------------------
  
  output$tablesummary <- DT::renderDataTable({ 
    
    DT::datatable(nodeData %>% 
                    select(c(`id`, `Category`, 
                             `Stage`)) %>%
                    group_by(!!sym(input$groupby)) %>%
                    #group_by(Profession, `Grade (Organogram)`) %>%
                    summarise(n())
    )
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)