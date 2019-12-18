# 1 Test Data ----

output$testdata_tab <- reactable::renderReactable({
  mydata <- testdata()
  
  return(
    reactable::reactable(
      mydata,
      bordered = TRUE,
      minRows = 5,
      sortable = TRUE,
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      pagination = TRUE,
      paginationType = "numbers",
      showPageSizeOptions = TRUE,
      highlight = TRUE,
      striped = TRUE,
      outlined = TRUE,
      compact = TRUE,
      wrap = FALSE,
      showSortIcon = TRUE,
      showSortable = TRUE
    )
  )
  
})



# csv data ----

output$uploaded_csv_data_tab <-
  reactable::renderReactable({
    mydata <- uploaded_csv_data()
    
    return(
      reactable::reactable(
        mydata,
        bordered = TRUE,
        minRows = 5,
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        paginationType = "numbers",
        showPageSizeOptions = TRUE,
        highlight = TRUE,
        striped = TRUE,
        outlined = TRUE,
        compact = TRUE,
        wrap = FALSE,
        showSortIcon = TRUE,
        showSortable = TRUE
      )
    )
    
  })


# Excel Data Preview ----

output$uploaded_excel_data_tab <-
  reactable::renderReactable({
    mydata <- uploaded_excel_data()
    
    return(
      reactable::reactable(
        mydata,
        bordered = TRUE,
        minRows = 5,
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        paginationType = "numbers",
        showPageSizeOptions = TRUE,
        highlight = TRUE,
        striped = TRUE,
        outlined = TRUE,
        compact = TRUE,
        wrap = FALSE,
        showSortIcon = TRUE,
        showSortable = TRUE
      )
    )
    
  })



output$dfshow_modified <-
  reactable::renderReactable({
    req(input$survivaltime)
    
    
    
    
    mydata <- dataSurvival()
    
    return(
      reactable::reactable(
        mydata,
        bordered = TRUE,
        minRows = 5,
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        paginationType = "numbers",
        showPageSizeOptions = TRUE,
        highlight = TRUE,
        striped = TRUE,
        outlined = TRUE,
        compact = TRUE,
        wrap = FALSE,
        showSortIcon = TRUE,
        showSortable = TRUE
      )
    )
    
    
    
    
    
    
  })





# 4 Correlation Matrix ----

output$corr_matrix <- renderTable({
  
  mydata <- corr_matrix_data()
  
  corr <- correlation::correlation(mydata)
  
  return(corr)
  
})