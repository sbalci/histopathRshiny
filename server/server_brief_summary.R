# 2 Func: Data Frame Summary ----

output$dfsummary <- renderPrint({
  mydata <- briefSummaryData()
  names(mydata)

  # https://www.statology.org/r-find-columns-with-all-na/


  # #check if each column has all missing values
  # all_miss <- apply(df, 2, function(x) all(is.na(x)))
  #
  # #display columns with all missing values
  # names(all_miss[all_miss>0])
  #
  # Method 2: Use purrr Package
  #
  # library(purrr)
  #
  # #display columns with all missing values
  # df %>% keep(~all(is.na(.x))) %>% names
  #

  # https://stackoverflow.com/questions/46428941/r-find-columns-where-all-values-are-either-na-or-single-value-0-variance


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
