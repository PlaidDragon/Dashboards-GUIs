
library(dplyr)
library(RMySQL)
library(tidyverse)
library(rio)
library(readxl)
library(dplyr)
library(RCurl)
library(xml2)
library(RODBC)
library(openxlsx)
library(rhandsontable)
library(shiny)

#Get user-defined functions that do stuff.
source("//fileserver//medical//TrackTraceRx//MATTISOAPFunctions.R")
holding_fame <- data.frame("ID" = (1:25)) %>% mutate("ID" = "")

ui <-  navbarPage(
  title = "MAS To TrackTrace Interface (MATTI)",

      tabPanel('Enter Sales IDs',
               sidebarPanel(
                 
                 
                 tags$head(tags$style("#generate{background:lightblue; color:black; font-style:bold;}")),
                 helpText("Please input the SOs or POs that need to be uploaded to TrackTrace. \n Inputs must be in \"PO-BA\" or \"SO-\" 
                        format that matches MAS. Both SOs and POs may be entered at the same time"),
                 actionButton("generate","Pull info from MAS"),
                 helpText("NOTE: This is a live version of MATTI that pushes to TrackTrace. (This is NOT the training version)."),
                 verbatimTextOutput("status")),
               mainPanel(
               rHandsontableOutput('ids_table'))
          ),
               
                
  tabPanel('Verify POs',
           helpText("Make adjustments to the inbound data as needed. Check the 'Verify' box on the data that you wish to send to TrackTrace and then press the button to send to TrackTrace"),
           
           tags$head(tags$style("#PO_Push{background:lightblue; color:black; font-style:bold;}")),
           actionButton("PO_Push","Push POs To TrackTrace"),
           
           
           
           hr("Pulled Inbound Data From MAS"),
           rHandsontableOutput('POtable'),
           hr("TrackTrace IDs"),
           tableOutput("TTPOIDS")
  ),
  tabPanel('Verify SOs',
           helpText("Make adjustments to the outbound data as needed. Check the 'Verify' box on the data that you wish to send to TrackTrace and then press the button to send to TrackTrace"),
           
           tags$head(tags$style("#SO_Push{background:lightblue; color:black; font-style:bold;}")),
           actionButton("SO_Push","Push SOs To TrackTrace"),
           
           
           hr("Pulled Outbound Data From MAS"),
           rHandsontableOutput('SOtable'), 
           hr("TrackTrace IDs"),
           tableOutput("TTSOIDS")
            
  ))
  



server <- function(input, output) {
  values <- reactiveValues(data = holding_fame)
  inputvalues <- reactiveValues(povals = NULL, sovals = NULL)
  
  observeEvent(input$generate, { 
    
    
    
    showModal(modalDialog("Pulling Data...This Can Take a While", footer=NULL))
    sale_ids <- as.data.frame(hot_to_r(input$ids_table))
    ids <- c(as.character(sale_ids$ID))
    
    
    warnings <- capture.output(inputIDs <- MATTIVerifyTransaction(ids), type = "message")
    
    w <- unique(warnings)
    w <- w[!is.na(w)]
    for(i in 2:length(w)){
      if(!is.null(w)){
        showNotification(w[i], type = "error", duration = 30)}
    }
    
    
    inputvalues$povals$data <- inputIDs$POs 
    inputvalues$sovals$data <- inputIDs$SOs
    
    
    output$status <- renderText({"Done"})
    
    inputvalues$povals$data <- (inputvalues$povals$data)
    inputvalues$sovals$data <- (inputvalues$sovals$data)
    
    removeModal()
    
  })
  
  
  observeEvent(input$PO_Push, {
    
    showModal(modalDialog("Sending POs to TrackTrace...", footer=NULL))
    po_ids <- as.data.frame(hot_to_r(input$POtable)) %>% filter(Verified == TRUE & `Not in TrackTrace` == TRUE)
    suppressMessages(write.xlsx(po_ids,"//fileserver//medical//TrackTraceRx//po_ids_test.xlsx"))
    
    
    powarnings <- capture.output(TTIDSPO <- MATTICreateInboundTransaction(po_ids), type = "message")
    output$TTPOIDS <- renderTable(TTIDSPO)
    
    
    
    removeModal()
    
    if(is.null(TTIDSPO)){
      showModal(modalDialog(paste0("THERE HAS BEEN AN ERROR: \n ",powarnings[length(powarnings)]), footer=NULL, easyClose = T))
      
    }
    
    pow <- unique(powarnings)
    pow <- pow[!is.na(pow)]
    for(i in 1:length(pow)){
      if(!is.null(pow)){
        showNotification(pow[i], type = "error", duration = 30)}
    }
    
    
    
  })
  
  
  observeEvent(input$SO_Push, {
    
    
    showModal(modalDialog("Sending SOs to TrackTrace...", footer=NULL))
    so_ids <- as.data.frame(hot_to_r(input$SOtable)) %>% filter(Verified == TRUE & `Not in TrackTrace` == TRUE)
    
    #suppressMessages(write.xlsx(so_ids,"//fileserver//medical//TrackTraceRx//so_ids_test.xlsx"))
    
    warnings <- capture.output(TTIDSSO <- MATTICreateOutboundTransaction(so_ids), type = "message")
    output$TTSOIDS <- renderTable(TTIDSSO)
    
    
    removeModal()
    
    
    
    w <- unique(warnings)
    w <- w[!is.na(w)]
    for(i in 1:length(w)){
      if(!is.null(w)){
        showNotification(w[i], type = "error", duration = 30)}
    }
    
  })
  
  
  
  output$POtable <- reactive({
    validate(
      need(inputvalues$povals$data, "Please Pull Data From MAS"))
    
    output$POtable <- renderRHandsontable({rhandsontable(inputvalues$povals$data , selectCallback = T, readOnly = F)})
  })
  
  
  
  output$SOtable <- reactive({
    validate(
      need(inputvalues$sovals$data, "Please Pull Data From MAS"))
    
    output$SOtable <- renderRHandsontable({rhandsontable(inputvalues$sovals$data, selectCallback = T, readOnly = F)}) 
  })
  
  output$ids_table <-renderRHandsontable({rhandsontable(values$data, selectCallback = T, readOnly = F)})
  
}



# Run the app ----
app <- shinyApp(ui = ui, server = server)

runApp(app, launch.browser = T)