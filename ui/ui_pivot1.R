# https://github.com/ablack3/shinyPivot

fluidPage(title = "R pivot table", 
          tabsetPanel(
            tabPanel("Local pivot", pivot_module_UI(id = "id", pivot_vars = pivot_vars))
          )
)



