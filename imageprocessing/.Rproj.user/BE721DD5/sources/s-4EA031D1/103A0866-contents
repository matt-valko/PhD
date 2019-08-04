
library('EBImage')


img<-readImage('~/Documents/Github/PhD//imageprocessing/burseragrav1.jpg')

outputDir<-getwd()


saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}


library('shiny')


ui <- fluidPage(


             h4("Brush and double-click to zoom"),
             plotOutput("plot1", height = 300,
                        dblclick = "plot1_dblclick",
                        brush = brushOpts(
                          id = "plot1_brush",
                          resetOnNew = TRUE), width = "100%")
              ,           
            
              selectInput("select", h3("Select box"), 
            choices = list("Leaf" = 1, "Stem" = 2,
                           "Flower" = 3,"Fruit" = 4,"Measure" = 5), selected = 1)
            
            ,      textOutput("summarize")

            ,actionButton("action", "Save"),
            actionButton("set Measure", "Set Measure")
            )
            
    


server <- function(input, output) {
  
  # -------------------------------------------------------------------
  # Single zoomable plot (on left)
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    if(!is.null(ranges$x)){
plot(img[ranges$x[1]:ranges$x[2],ranges$y[1]:ranges$y[2],])} else{plot(img)}
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  
    output$summarize <- renderText({ 
      if(!is.null(ranges$x)){
        subimg<-img[ranges$x[1]:ranges$x[2],ranges$y[1]:ranges$y[2],]  
        subimgray<-channel(subimg,"gray")
        y = gblur(crop, 3) > .8
        
        y<-bwlabel(y)
        paste(c('area','perimeter'),computeFeatures.shape(y)[,c(1,2)])
      }
      else{'No Image selected'}
    })
  
    observeEvent(input$action,{saveData(c(ranges$x,ranges$y,input$select))})
}

shinyApp(ui, server)
