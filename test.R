# library(shiny)    
# runGitHub("coronaOverview", "ChristophAnten") 




ui <- shinyUI(fluidPage(    
  tags$head(
    tags$script(
      HTML("
           // Get mouse coordinates
           var mouseX, mouseY;
           $(document).mousemove(function(e) {
           mouseX = e.pageX;
           mouseY = e.pageY;
           }).mouseover();

           // Function to position draggable, place on current mouse coordinates
           Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
           var element = $('#click_info').parent();
           element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})
           });
           ")
    )
  ),
  absolutePanel(fixed=TRUE, draggable = TRUE, uiOutput("click_info")),
  plotOutput("myplot", click = clickOpts(id ="myplot_click"))
))

server <- shinyServer(function(input, output, session) {
  
  output$myplot <- renderPlot({
    ggplot(mtcars) + geom_point(aes(mpg,cyl))
  })
  
  show_this  = reactiveVal(NULL)
  print_this = reactiveVal(NULL)
  observeEvent(input$myplot_click, {
    p <- nearPoints(mtcars, input$myplot_click, maxpoints=1)
    if( nrow(p) == 0 ) {
      show_this(NULL)
    }
    else {
      session$sendCustomMessage(type = 'placeDraggable', message = list())
      show_this(tagList(
        { actionButton("input_button","OK") },
        { br() },
        { verbatimTextOutput("point_info") }
      )
      )
      print_this({p})
    }
  })
  output$click_info <- renderUI   (show_this() )
  output$point_info <- renderPrint(print_this())
  
  observeEvent(input$input_button,{
    if (input$input_button) { show_this(NULL) }
  })
  
})

shinyApp(ui, server)
