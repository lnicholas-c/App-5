library(shiny)
library(markdown)

medAvailability <- read.csv("generic_medicine_availability.csv")
medPrice <- read.csv("generic_medicine_price_ratio.csv")
availabilityData <- medAvailability[c(2:16, 18:20, 22:39), ]
priceData <- medPrice[c(2:3, 5:7, 9, 11:12, 15:18, 24, 26:32), ]
privateAvail <- c(availabilityData$Median.availability.of.selected.generic.medicines.......Private)
publicAvail <- c(availabilityData$Median.availability.of.selected.generic.medicines.......Public)
privatePrice <- c(priceData$Median.consumer.price.ratio.of.selected.generic.medicines...Private)
publicPrice <- c(priceData$Median.consumer.price.ratio.of.selected.generic.medicines...Public)


ui <- fluidPage(
  
  navbarPage("Generic Medicine Data By Country: 2007-2013",
             tabPanel("Availability",
                titlePanel("Median Availability of Selected Generic Medicines"),
                    sidebarLayout( 
                       sidebarPanel(
                          p("Click the button to view a summary of the graph"),
                          actionButton("sumAvail", "Summary"),
                          verbatimTextOutput("textAvail")),
                       
                       mainPanel(
                         plotOutput("availability", width = "700px", height = "700px")
                       )
                    )   
                 ),
             
             tabPanel("Consumer Price Ratio",
                titlePanel("Median Consumer Price Ratio of Selected Generic Medicines"),
                   sidebarLayout(
                      sidebarPanel(
                         p("Click the button to view a summary of the graph"),
                         actionButton("sumPrice", "Summary"),
                         verbatimTextOutput("textPrice")),
                      
                      mainPanel(
                        plotOutput("priceRatio", width = "700px", height = "700px")
            )
         )
      )
   )
)

server <- function( input, output ) {
  
  output$availability <- renderPlot ({
    plot( privateAvail, publicAvail, 
          xlab = "Median Private Availability Percentage",
          ylab = "Median Public Availability Percentage")
    abline(lm(publicAvail~privateAvail, col = "blue"))
  })
  
  output$priceRatio <- renderPlot ({
    plot( privatePrice, publicPrice, 
          xlab = "Median Private Consumer Price Ratio", 
          ylab = "Median Public Consumer Price Ratio")
    abline(lm(publicPrice~privatePrice, col = "blue"))
  })
  
  availButton <- eventReactive(input$sumAvail)
  
  priceButton <- eventReactive(input$sumPrice)
  
  
  output$textAvail <- renderPrint ({
    availButton()
    
  })
  
  output$textPrice <- renderPrint ({
    priceButton()
    
  })
}

shinyApp( ui = ui, server = server )