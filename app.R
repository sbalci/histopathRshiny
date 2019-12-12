# Pre-Shiny Code ----

options(shiny.autoload.r = TRUE)

# mydata <- readxl::read_xlsx(path = here::here("mydata.xlsx"))

mydata <- readxl::read_xlsx(path = here::here("mydata.xlsx"))

# mydata_modified <- readxl::read_xlsx(path = here::here("mydata_modified"))

exampleData <- mydata


# UI ----


ui <- navbarPage(
  title = "histopathR",
  inverse = TRUE,
  collapsible = TRUE,
  selected = "Data Select & Upload",
  fluid = TRUE,
  windowTitle = "Histopathology Research Template & Analysis",
  header = "Histopathology Research Template & Analysis",
  
  # Shiny Extensions & Style ----
  
  theme = shinythemes::shinytheme("spacelab"),
  shinyFeedback::useShinyFeedback(),
  tags$style("#switcher { display:none; }"),
  
  
  p("Write Here what is this about"),
  tags$hr(),
  
  
  
  # Page: Data Select & Upload ----
  
  tabPanel(
    "Data Select & Upload",
    
    
    # Row: First Page First Row ----
    
    fluidRow(
      column(
        4,
        tags$h3("Download Data"),
        tags$br(),
        tags$br(),
        
        
        # Download: Fake Example Dataset download button ----
        
        tags$b("Download Fake Example Data"),
        tags$br(),
        tags$br(),
        
        
        
        
        downloadButton(outputId = "downloadExample", label = "Download Fake Excel"),
        
        tags$br(),
        tags$br(),
        tags$hr(),
        
        
        
        
        # Download: Modified Dataset download button ----
        
        tags$b("Download Modified Data"),
        tags$br(),
        tags$br(),
        
        
        downloadButton(outputId = "downloadModified", label = "Download Modified Excel"),
        tags$br(),
        tags$br(),
        tags$hr()
        
        
        
        
      ),
      column(
        4,
        tags$h3("Upload Your File"),
        tags$br(),
        tags$br(),
        
        
        
        # Input: Upload a file ----
        
        fileInput(
          inputId = "file1",
          label =  "Upload your own CSV File",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        
        # Horizontal line
        tags$hr(),
        
        # Input: Checkbox if file has header
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator
        radioButtons(
          "sep",
          "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ";"
        ),
        
        # Input: Select quotes
        radioButtons(
          "quote",
          "Quote",
          choices = c(
            None = "",
            "Double Quote" = '"',
            "Single Quote" = "'"
          ),
          selected = '"'
        ),
        
        
        
        
        
      ),
      column(
        4,
        tags$h3("Preprocess"),
        tags$br(),
        tags$br(),
        p("Select Columns like name, id"),
        p("Select Columns you do not want in the analysis"),
        
        
        
        # Select ID columns ----
        
        textOutput("namesdatatext"),
        
        selectizeInput(
          inputId = 'IDCols',
          'Select ID columns',
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        
        
        
        
        
        
        
        
        
        
        
      ),
      
    ),
    
    
    
    
    tags$hr(),

    # Row: First Page Second Raw ----
    
        
    fluidRow(
      column(
        6,
        tags$h3("Uploaded Data"),
        
        
        
        
        
        # Output: show data ----
        

        reactable::reactableOutput("dfshow"),
        
        
        
        
        
        
      ),
      column(
        6,
        tags$h3("Modified Data"),
        
        
        
        
        
        # Output: show data modified ----
        

        reactable::reactableOutput("dfshow_modified"),
        
        
        
      )
    )
    
    
    
    
    
    
    
    
    
  ),
  
  
  
  
  # Page: Brief Summary ----
  
  
  tabPanel(
    "Brief Summary",
    
    
    
    
    
    
    
    # Output: DataFrame Summary ----
    
    titlePanel("Data Summary"),
    verbatimTextOutput("dfsummary"),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  ),
  
  
  
  # Pages: Descriptives ----
  
  
  navbarMenu(
    "Descriptives",
    
    
    # Pages: Continious ----
    
    
    tabPanel("Continious"),

    # Pages: Categorical ----
    
    
    tabPanel("Categorical"),
    
    tabPanel("EDA1"),
    
    tabPanel("EDA2")
    
    
  ),
  
  # Pages: Correlation ----
  
  
  tabPanel(
    "Correlation"
    
    
    
    
  ),
  
  
  
  # Pages: ROC ----
  
  
  tabPanel(
    "ROC"
    
    
    
    
  ),
  
  
  
  
  
  # Page: Survival ----
  
  
  tabPanel(
    "Survival",
    
    
    
    
    
    ## Sidebar layout with input and output definitions ----
    
    sidebarLayout(
      fluid = TRUE,
      position = "right",
      
      # Sidebar panel for inputs ----
      
      sidebarPanel(
        # Input: Select Factor ----
        selectInput(
          inputId = "Factor",
          label = "Choose a Factor Affecting Survival",
          choices = c("LVI", "PNI"),
          
          # Get choices list from names(df) %>% categorical
          
          selected = "LVI"
        ),
        
        # Input Comments ----
        
        textAreaInput(
          inputId = "comment",
          label = "Comments",
          placeholder = "Add comments here"
        ),
        
        
        
        
        
        
        
      ),
      
      # Main panel for displaying outputs ----
      
      mainPanel(
        # Output: Kaplan-Meier ----
        
        tags$br(),
        tags$h3("Kaplan-Meier Plot, Log-Rank Test"),
        tags$br(),
        tags$hr(),
        plotOutput("KMPlot"),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        tags$h3("Univariate Cox-Regression"),
        tags$br(),
        tags$hr(),
        tableOutput("CoxTable"),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        tags$h3("Median Survival"),
        tags$br(),
        tags$hr(),
        tableOutput("Median"),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        tags$h3("1-3-5-yr Survival"),
        tags$br(),
        tags$hr(),
        tableOutput("YearSurv"),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        tags$h3("Comment 1-3-5-yr Survival"),
        tags$br(),
        tags$hr(),
        textOutput("YearSurvComment"),
        tags$br(),
        tags$br(),
        tags$hr(),
        tags$br(),
        
        # Output: comment ----
        
        tags$h3("My Comments"),
        textOutput("comment")
        
        
      )
    ),
    
    
    
    
    
    
  ),
  
  
  # Pages: About ----
  
  
  navbarMenu(
    "About",
    
    
    
    tabPanel(
      title = "Project",
      
      
      includeHTML("https://sbalci.github.io/histopathology-template/")
      
      
      
      
      
      
      
      
      
      
    ),
    
    tabPanel("References")
    
    
  )
  
  
  
  
)




# Server ----

server <- function(input, output, session) {
  # Download Example Data ----
  
  
  output$downloadExample <- downloadHandler(
    filename = function() {
      paste0("fakedata.xlsx")
    },
    
    content = function(file) {
      rio::export(x = exampleData,
                  file = file,
                  format = "xlsx")
    }
    
  )
  
  
  # Upload csv File ----
  
  read_data <- reactive({
    if (is.null(input$file1)) {
      mydata
      
    } else {
      read.csv(
        file = input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote,
        na.strings = c("NA", "NaN", "")
      )
      
    }
  })
  
  # Names Data ----
  
  
  updateSelectizeInput(session,
                       'IDCols',
                       choices = names(mydata),
                       server = TRUE)
  
  
  
  
  # Func: show data ----
  
  output$dfshow <- reactable::renderReactable({
    mydata <- read_data()
    
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
  
  
  # Func: change data ----
  
  
  read_data2 <- reactive({
    req(input$IDCols)
    
    idcols <- input$IDCols
    
    read_data2 <- read_data() %>%
      select(-idcols)
    
  })
  
  
  
  
  read_data3 <- reactive({
    read_data3 <-
      
      read_data2() %>%
      dplyr::mutate(int2 = lubridate::interval(
        lubridate::ymd(SurgeryDate),
        lubridate::ymd(LastFollowUpDate)
      )) %>%
      mutate(OverallTime =
               lubridate::time_length(int2, "month")) %>%
      mutate(OverallTime =
               round(OverallTime, digits = 1))
    
  })
  
  
  
  
  
  # Func: show modified data ----
  
  output$dfshow_modified <- reactable::renderReactable({
    mydata <- read_data3()
    
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
  
  # Func: Data Frame Summary ----
  
  
  output$dfsummary <- renderPrint({
    mydata <- read_data3()
    
    myreport <-
      mydata %>%
      select(names(which(
        sapply(mydata, class) == "character" |
          sapply(mydata, class) == "numeric"
      ))) %>%
      report::report(.)
    
    cat(myreport[["text_full"]])
    
    
  })
  
  
  
  
  # Func: Kaplan-Meier ----
  
  output$KMPlot <-
    
    renderPlot({
      mydata <- read_data3()
      
      mydata$Outcome <- mydata$Death
      
      mydata$OverallTime <-
        as.numeric(as.character(mydata$OverallTime))
      
      explanatory <- forcats::as_factor(input$Factor)
      
      mydata %>%
        finalfit::surv_plot(
          .data = .,
          dependent = "Surv(OverallTime, Outcome)",
          explanatory = explanatory,
          xlab = 'Time (months)',
          pval = TRUE,
          legend = 'none',
          break.time.by = 12,
          xlim = c(0, 60)
        )
      
    })
  
  # Func: Cox-Regression ----
  
  
  output$CoxTable <-
    
    renderTable({
      mydata <- read_data3()
      
      mydata$Outcome <- mydata$Death
      
      
      mydata %>%
        finalfit::finalfit("Surv(OverallTime, Outcome)", input$Factor)
      
    })
  
  
  # Func: Median Survival ----
  
  
  output$Median <-
    
    
    renderTable({
      mydata <- read_data3()
      
      mydata$Outcome <- mydata$Death
      
      
      formula_text <-
        paste0("Surv(OverallTime, Outcome) ~ ", input$Factor)
      
      km_fit <- survfit(as.formula(formula_text),
                        data = mydata)
      
      km_fit_median_df <- summary(km_fit)
      
      km_fit_median_df <- as.data.frame(km_fit_median_df$table) %>%
        janitor::clean_names() %>%
        tibble::rownames_to_column()
      
    })
  
  
  # Func: Year Survival ----
  
  
  output$YearSurv <-
    
    renderTable({
      mydata <- read_data3()
      
      mydata$Outcome <- mydata$Death
      
      
      formula_text <-
        paste0("Surv(OverallTime, Outcome) ~ ", input$Factor)
      
      km_fit <- survfit(as.formula(formula_text),
                        data = mydata)
      
      km_fit_summary <- summary(km_fit, times = c(12, 36, 60))
      
      km_fit_df <-
        as.data.frame(km_fit_summary[c("strata",
                                       "time",
                                       "n.risk",
                                       "n.event",
                                       "surv",
                                       "std.err",
                                       "lower",
                                       "upper")])
      
      km_fit_df
      
    })
  
  
  # Func: YearSurvComment ----
  
  
  
  output$YearSurvComment <-
    
    renderPrint({
      mydata <- read_data3()
      
      mydata$Outcome <- mydata$Death
      
      
      formula_text <-
        paste0("Surv(OverallTime, Outcome) ~ ", input$Factor)
      
      km_fit <- survfit(as.formula(formula_text),
                        data = mydata)
      
      km_fit_summary <- summary(km_fit, times = c(12, 36, 60))
      
      km_fit_df <-
        as.data.frame(km_fit_summary[c("strata",
                                       "time",
                                       "n.risk",
                                       "n.event",
                                       "surv",
                                       "std.err",
                                       "lower",
                                       "upper")])
      
      km_fit_df %>%
        dplyr::mutate(
          description =
            glue::glue(
              "When {strata}, {time} month survival is {scales::percent(surv)} [{scales::percent(lower)}-{scales::percent(upper)}, 95% CI]."
            )
        ) %>%
        dplyr::select(description) %>%
        pull() -> comment
      
      print(comment)
      
    })
  
  
  
  
  # Func: Comment ----
  
  output$comment <- renderText(input$comment)
  
  
  # Download Modified Data ----
  
  
  output$downloadModified <- downloadHandler(
    filename = function() {
      paste0("modifiedData.xlsx")
    },
    
    
    content = function(file) {
      rio::export(x = read_data3(),
                  file = file,
                  format = "xlsx")
    }
    
  )
  
  
}




# Run the Shiny app ----

shinyApp(ui, server)
