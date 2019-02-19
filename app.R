#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Maize and Urea rasters"),
   
   # Sidebar with a slider input for number of bins 
   radioButtons(inputId = "raster", label = "", 
                choices = list( "Predicted Maize Prices" = "maizeraster",
                                "Predicted Fertilizer Prices" = "fertpriceraster",
                                "Actual UREA Prices" = "UREApriceraster",
                                "Relative UREA Prices" = "UreaRtvPriRaster"), 
                selected = NULL,
                inline = TRUE, width = NULL),
   sidebarLayout(
      sidebarPanel(
         sliderInput("price",
                     "Percentage change in Price Map Values:",
                     min = 1,
                     max = 300,
                     value = 100),
         downloadButton("downloadData1", "Download Price Map"),
           sliderInput("price_abv",
                       "Set Minimum Price Shown:",
                       min = 1,
                       max = 300,
                       value = 100),
         # Download Button
         downloadButton("downloadData2", "Download Minimum Price Map")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("rasterPlot"),
         plotOutput("rasterPlot2")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  rasterfiles <- list( 
    fertpriceraster = raster("./data/pred_fert_price1.tif"),
    maizeraster = raster("./data/pred_maize_price1.tif"),
    UREApriceraster = raster("./data/TZA_price.tif"),
    UreaRtvPriRaster = raster("./data/tza_rel_pr.tif")
    )
  maintitles <- list( 
    fertpriceraster = "Predicted Fertilizer Prices",
    maizeraster = "Predicted Maize Prices",
    UREApriceraster = "Actual UREA Prices",
    UreaRtvPriRaster = "Relative UREA Prices"
    )
  
  observeEvent(input$raster, {
    minopt <- ceiling(rasterfiles[[input$raster]]@data@min)
    maxopt <- floor(rasterfiles[[input$raster]]@data@max)
    updateSliderInput(session, "price_abv", value = NULL,
                      min = minopt, max = maxopt, step = NULL)
  })

   output$rasterPlot <- renderPlot({
     plot(rasterfiles[[input$raster]]*input$price/100, main=maintitles[[input$raster]])
   })
   
   output$rasterPlot2 <- renderPlot({
     tempraster <- rasterfiles[[input$raster]]
     tempraster[tempraster < input$price_abv] <- NA
     plot(tempraster, main=paste0(maintitles[[input$raster]], " : Prices Above TSH", input$price_abv))
   })
   
   # Downloadable Raster of selected dataset ----
   output$downloadData1 <- downloadHandler(
     filename = function() {
       paste(input$raster, input$price, as.numeric(Sys.time()), ".tif", sep = "")
     },
     content = function(file) {
       writeRaster(rasterfiles[[input$raster]]*input$price/100, file, row.names = FALSE)
     }
   )
   output$downloadData2 <- downloadHandler(
     filename = function() {
       paste(input$raster, "PriceAbove", input$price_abv, as.numeric(Sys.time()), ".tif", sep = "")
     },
     content = function(file) {
       tempraster <- rasterfiles[[input$raster]]
       tempraster[tempraster < input$price_abv] <- NA
       writeRaster(tempraster, file, row.names = FALSE)
     }
   )
   
}

# Run the application 
shinyApp(ui = ui, server = server)

