library(shiny)
library(rhandsontable)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(stringr)
library(plotly)
library(tidyr)



ui <- fluidPage(
  useShinydashboard(),
  
  # Give the page a title
  titlePanel("Optimal's Comet Score"),
  
  #Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar
    sidebarPanel(width = 4,
                 helpText("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus diam felis, sollicitudin quis faucibus in,
                 pretium et metus. In hac habitasse platea dictumst. Ut finibus nisi nec diam accumsan, at egestas augue porttitor.
                 Quisque nec tortor iaculis, dignissim magna vitae, congue justo. Nam id bibendum diam. Maecenas odio quam, tincidunt
                 vitae eros at, sollicitudin finibus ipsum. Sed at rutrum ipsum, vel sodales purus. Cras pellentesque sem magna, nec
                 sollicitudin mi ullamcorper et. Donec ex neque, condimentum in metus vitae, ultrices dignissim risus. Fusce ultricies
                 suscipit purus eu sagittis.Proin varius diam mi, eu tincidunt augue mattis non. Donec accumsan, purus consectetur
                          pretium consequat, ligula odio aliquet tellus, ac bibendum mi mi non mi.",br(),br(),br(),"
                          Vivamus tincidunt nunc varius nulla
                          venenatis, a tempor quam rutrum. Nam nulla felis, varius eget blandit eu, dictum sagittis ligula. Sed blandit
                          varius laoreet. Nulla vel lorem non nibh molestie vulputate. Sed accumsan mauris lectus, non consectetur ipsum 
                          sodales convallis. Vivamus cursus tellus ipsum, eget sollicitudin lacus elementum eget. Morbi eu mollis purus,
                          nec tempus sem. Suspendisse potenti. Duis commodo erat sit amet odio aliquam laoreet. Ut dolor libero,
                          elementum vitae libero posuere, vestibulum feugiat quam. Aliquam vel augue vel lectus egestas vehicula quis 
                          quis eros. Proin eget nisl elit. In eu purus pulvinar, rutrum dolor ac, auctor risus.")
                  ),
    mainPanel(
      tabsetPanel(
        tabPanel("Comet Score",
                 fluidRow(
                   column(5,
                          hr()
                   )
                 ),
                 fluidRow(
                   column(5,
                          selectizeInput(inputId = "url",
                                    label = "Enter Your URL",
                                    choices = NULL
                                    )
                   )
                 ),
                 fluidRow(
                   column(5,
                          selectizeInput(inputId = "device",
                                         label = 'Device',
                                         choices = c('Desktop','Phone'),
                                         selected = 'Desktop'
                                   ) 
                          ),
                   column(4,
                          selectizeInput(inputId = "country",
                                         label = 'Country',
                                         choices = c('United States'),
                                         selected = 'United States'
                          )
                   ),
                 ),
                 actionBttn("calculate", label = "Calculate Score", size = "m", style = "jelly"),
                 br(),br(),
                 fluidRow(
                   p(),
                   column(4, p(), htmlOutput("percText"), offset=4),
                   column(4, p(),valueBoxOutput("scoreBox", width = "100%"), offset = 4),
                   column(4, p(), htmlOutput("contact"), offset=4),
                   ),
                 br(),br(), br(),
                 fluidRow(
                   plotlyOutput("barPlot")
                 ),
                 tags$head(tags$style("#contact{color: black;
                                 font-size: 30px;
                                 font-style: bold;
                                 }"
                 ))
                 
        ),
        tabPanel("Advanced Settings",
                 fluidRow(
                   column(6,
                          h2("Manual CDF Selection"),
                          checkboxInput('manualGroup',
                                        label = "Define Site Make up Manually",
                                        value = FALSE
                                        ),
                          selectizeInput(inputId = "group",
                                         label = 'What Most Accurately Describes your Website?',
                                         choices = c('Low Overall Content Size, Few Scripts', 'Low Number of Scripts, High Content Size','Many Scripts, Low Overall Content Size')
                                        )
                          ),
                   column(6,
                          h2("Custom Competitors"),
                          selectizeInput(inputId = "comps",
                                         label = 'Choose who you Want to be Shown on the Distribution Plot',
                                         choices = c("",unique(holdPoints$origin[holdPoints$country_code == 'us'])),
                                         multiple = TRUE,
                                         options = list(maxItems = 5)),
                          helpText("Note: Only 5 will be shown")
                          )
                   )
        ),
        tabPanel("About",
                 helpText("Here's the high level math behind the Comet Score: It is losely based off of Google's Lighthouse Score which uses the Abramowitz and Stegun formula
                            to derive a log-normal cumulative distribution function that can ultimately be used to derive a score of 0-1 from a given timing metric. Google uses
                            a weighted average of several of these metrics to derive a site's ultimate Lighthouse Performance Score."),
                 br(),
                 helpText("We wanted to create a score that more accurately reflects how Real Users experience a website, so we used some fancy Artificial Intelligence
                            to determine what sites are similar in make up to create a log-normal CDF, like the one shown below, that is catered to specific sites, so the score more accurately reflects a scale
                            of how you compare with your competitors specifically, using real measurment values gathered from Google Crux."),
                 br(),
                 helpText("The Comet score is calculated using the following metrics and weights:"),
                 br(),
                 helpText("First Input Delay: 30%"),
                 helpText("Largest Contentful Paint: 25%"),
                 helpText("Time to First Byte: 20%"),
                 helpText("Cumulative Layout Shift: 15%"),
                 helpText("First Contentful Paint: 10%"),
                 br(),
                 helpText("Are you ready to improve your site speed performance? Call Optimal today!"),
    
                 fluidRow(h2("CDF Distributions"),
                   column(6,
                          plotlyOutput("cdfPlots")
                   )
                 )

        )
      )
    )))



server <- function(input, output, session){
  
  updateSelectizeInput(session = session, inputId = 'url', label = "Enter Your URL", choices = c("",unique(holdPoints$origin[holdPoints$country_code == 'us'])), selected = NULL, server = TRUE)

  
  observeEvent(input$calculate, {
    
    
    defaultDat <- holdPoints %>%
      filter((origin == input$url) & (device == tolower(input$device)) & (country_code == 'us')) %>%
      distinct(origin, .keep_all = TRUE)
    print(defaultDat)
    
    tryCatch({
      
      if(input$manualGroup == TRUE){group <- case_when(
                                              input$group == 'Low Overall Content Size, Few Scripts' ~ 1L,
                                              input$group == 'Low Number of Scripts, High Content Size' ~ 2L,
                                              input$group == 'Many Scripts, Low Overall Content Size' ~ 3L,
                                              TRUE ~ defaultDat$Cluster
                                              )

        
      }else{
          group <- defaultDat$Cluster
      }
    
      #------------UPDATE THIS FUNCTION TO SHOW COMPS AS WELL AS MANUAL CLUSER------------
      scoreStuff <- getMetricScores(url =  input$url, cluster = group, device_in = tolower(input$device),
                                    country = 'us', compUrls = input$comps,
                                    tenths = tenths, holdpoints = holdpoints, medians = medians)
      
      output$barPlot <- renderPlotly({scoreStuff$scoreHist})
      output$cdfPlots <- renderPlotly({scoreStuff$cdfPlots[[1]]})
      output$scoreBox <- renderValueBox({valueBox(scoreStuff$Score, "Comet Score", icon = icon("star"), color = "light-blue")})
      output$percText <- renderText(paste0('Users on your site experience speeds faster than ', scoreStuff$Percentile,'% of similar sites'))
      output$contact <- renderText("Want to Improve?<br>Contact Optimal today!")
    }, error=function(e){
        showNotification(paste0("Data for ", input$url, " is unavailable for ", input$device, " platform"), type = "error", duration = 7)
  })
})
}

#Call application
shinyApp(ui = ui, server = server)


