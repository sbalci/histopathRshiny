# 1 genUI: Upload spss Data ----

output$upload_spss_ui <- renderUI({
  if (is.null(input$select_data_ui))
    return()
  
  if (input$select_data_ui == "spss_ui")
    
    
    wellPanel(p("Work in progress SPSS")
              
              # TODO
    )
  
})
