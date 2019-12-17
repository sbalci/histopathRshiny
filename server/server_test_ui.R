# 1 genUI: Test Data ----

output$test_ui <- renderUI({
  if (is.null(input$select_data_ui))
    return()
  
  if (input$select_data_ui == "testdata_ui")
    
    wellPanel(
      p("The analysis will be made with an example data"),
      p("You may download the data to see how an organized data should be"),
      
      tags$b("Download Fake Example Data"),
      tags$br(),
      tags$br(),
      
      downloadButton(outputId = "downloadExample", label = "Download Fake Excel"),
      
      tags$br(),
      tags$br(),
      
      actionButton(inputId = "test_Button", "Approve"),
      p("Click the button to approve using data below"),
      
      tags$br(),
      tags$br(),
      
      tags$h3("Test (Example) Data"),
      reactable::reactableOutput("testdata_tab")
      
      
    )
})
