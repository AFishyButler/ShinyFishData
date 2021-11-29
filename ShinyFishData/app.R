
library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(ggthemes)
CTmax_swim_Fry <- read_csv("CTmax_swim_Fry.csv", 
                           col_types = cols(`Fish ID` = col_character(),
                                            `dropout time` = col_time(format = "%H:%M:%S"), 
                                            `Acclimation temp C` = col_character(), 
                                            `fork length (mm)` = col_double(),
                                            `Mass (g)`= col_double(),
                                            `Dropout temp` = col_double()))

ui <- fluidPage( theme = shinytheme("sandstone"),
                 tags$head(
                     tags$style(HTML("
    img {
      display: block;
      height: auto;
      margin: 0;
      margin-bottom: 1rem;
      padding: 0;
      width: 100%;
    }"))
                     ),          
                 
    h1(div("Fishy data app"), style = "color:green; font-family: 'times'"),
#Using a sidebar layout helps with organization; the contents of the sidebar is relevant to different portions of the main panel, so having the side panel act as an anchor for the rest of the page keeps the amount of clicking required for interaction to a minimum.
     sidebarLayout(
       sidebarPanel(sliderInput("id_slider", "Select mass",
                                min = 0.2, max = 1, value = c(0.5, 0.6), post = " g"),
                    radioGroupButtons(
#Radio buttons allow the user to select specific temperature treatment types for the histograms
                        "id_radio", "Select acclimation temperature", 
                        choices= unique(CTmax_swim_Fry$`Acclimation temp C`),
                        status = "success"),
#I've included an image in the sidebar so the user will have an idea of what the fish used in the experiment looked like.
                    img(src = "Fish_swim.png"),
                    img(src = "swim_flume.png")

     ),
mainPanel(tabsetPanel 
          (uiOutput("myrandom"),
#Here I've decided to make tabs for each part of the interactivity. This helps keep the page organized and reduces the amount of scrolling the user would have to do to be able to see the aspects of the plots and tables
    tabPanel("Welcome",h3(div(p("This is an app that allows the exploration of mass, length, and swim performance of Chinook salmon under different thermal regimes. The video below demonstrates the swim trial."),
                              br(),
                              p("The slider on the right controls the range of the fish mass selected and the buttons control the acclimation temperature desired. Click one of the tabs to begin.")), style="color:green; font-family: 'times'"),
             br(),
#The video embedded demonstrates the swimming trial involved.
            tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/VHz4yGZHGiA", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA) ),
#The histograms and the table are all reactive to the slider and radio buttons, which allow for the user to have immediate feedback on the changes they've made to see the results in the data          
    tabPanel("Fork length", 
             br(),
             h4(p("With this histogram, you can sort through the data by looking at the distribution of fork lengths as a function of mass and acclimation temperature")), 
             br(),
             plotOutput(outputId="id_forklength")),
    tabPanel ("Drop out temperature by mass",
              br(),
             h4( p("With this histogram, you can sort through the data by looking at the distribution of dropout temperatures as a function of mass and acclimation temperature")),
             br(),
             plotOutput(outputId="id_dropouttemp")),
    tabPanel("Interactive table", 
             br(),
             h4(p("Here you can use the slider to sort through the data by mass and acclimation temperature. ")), 
             br(),
             tableOutput("id_table")),
#If the user would like to perform further work on the dataset, they can download it from here
    tabPanel("View the Data", 
             br(),
             h4(p("View the entire dataset and download here")), 
             downloadBttn('downloadData',
                          label = "Download",
                          style = "stretch",
                          color = "success",
                          size = "md",
                          block = FALSE,
                          no_outline = TRUE),
             br(),
             br(),
#Here the user is able to see the entire dataset, without any filters, and interact with it in ways they want to.
             dataTableOutput("CTmax_swim_Fry"),
             br(), 
             br(),
            ))
)
)
)

server <- function(input, output, session) {
       output$myrandom <- renderUI(
           plotOutput("id_forklength")
    )
    forklength_filtered <- reactive({
        CTmax_swim_Fry %>%
            filter(`Mass (g)` < input$id_slider[2],
                `Mass (g)` > input$id_slider [1],
                `Acclimation temp C` == input$id_radio )
    })
    
    wholeData <- reactive({
        CTmax_swim_Fry})

    output$id_forklength <- renderPlot(
        forklength_filtered()%>%
        ggplot(aes(`fork length (mm)`))+
            geom_histogram()+
            theme_solarized())
    
    output$id_dropouttemp <- renderPlot(
        forklength_filtered()%>%
            ggplot(aes(`Dropout temp`))+
            geom_histogram()+
            theme_solarized())
   
    output$id_table <- renderTable(forklength_filtered())
    
    output$CTmax_swim_Fry <- renderDataTable(wholeData())
    
    output$downloadData<- downloadHandler(
        filename = function(){paste ("CTmax_swim_Fry", "csv", sep = ".")},
        
        content = function(file){
            write.csv(wholeData(), file)
            
    }
        
    )
    
   }

# Run the application 
shinyApp(ui = ui, server = server)
