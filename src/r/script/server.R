server <- function(input, output, session) {
  session$onSessionEnded(stopApp)  
  
  output$plt <- renderUI({
    
    lapply(pairs, function(x){
      column(
        6
        ,renderPlot(height = 600,{
          plt.result(x)
        }
        )
      )
    })
    
  })
}
