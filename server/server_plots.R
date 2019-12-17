# 4 Correlation -----


output$scatter_plot <- renderPlot({
  ggplot(
    read_data(),
    aes_string(
      x = input$x_variable,
      y = input$y_variable,
      color = input$color_variable
      
    )
  ) +  
    geom_point()
  
})
