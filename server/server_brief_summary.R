# 2 Func: Data Frame Summary ----

output$dfsummary <- renderPrint({
  mydata <- briefSummaryData()
  names(mydata)
  myreport <-
    mydata %>%
    select(where(is.numeric) | where(is.character)) %>%
    # select(names(which(
    #   sapply(mydata, class) == "character" |
    #     sapply(mydata, class) == "numeric"
    # ))) %>%
    report::report(.)
  cat(myreport)
  # cat(myreport[["text_full"]])
})
