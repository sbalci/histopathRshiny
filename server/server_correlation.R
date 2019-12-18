# 4 Correlation Matrix ----

output$corr_matrix <- renderTable({
  
  mydata <- corr_matrix_data()
  
  corr <- correlation::correlation(mydata)
  
  return(corr)
  
})





## Correlation Analysis


# TODO ----

#  https://easystats.github.io/correlation/
# install.packages("devtools")
# devtools::install_github("easystats/correlation")
# library("correlation")
















# iris %>%
#   select(Species, starts_with("Sepal")) %>%
#   group_by(Species) %>%
#   correlation::correlation() %>%
#   filter(r < 0.9)




# correlation::correlation(select(iris, Species, starts_with("Sepal")),
#  select(iris, Species, starts_with("Petal")),
#  partial=TRUE)




# correlation(iris, bayesian=TRUE)


# library(report)
# iris %>%
#   select(starts_with("Sepal")) %>%
#   correlation::correlation(bayesian=TRUE) %>%
#   report()


# report::report(cor.test(iris$Sepal.Length, iris$Petal.Length))


# https://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.test.html


# iris %>%
#   group_by(Species) %>%
#   correlation() %>%
#   report() %>%
#   to_table()



# iris %>% explore(Sepal.Length, Petal.Length)

# iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
# iris %>% explore(Sepal.Length, Petal.Length, target = is_versicolor)


# dlookr::correlate(carseats)
# dlookr::correlate(carseats, Sales, CompPrice, Income)
# dlookr::correlate(carseats, Sales:Income)
# dlookr::correlate(carseats, -(Sales:Income))
# carseats %>%
#   dlookr::correlate(Sales:Income) %>%
#   dplyr::filter(as.integer(var1) > as.integer(var2))
# carseats %>%
#   dplyr::filter(ShelveLoc == "Good") %>%
#   group_by(Urban, US) %>%
#   dlookr::correlate(Sales) %>%
#   dplyr::filter(abs(coef_corr) > 0.5)


# dlookr::plot_correlate(carseats)
# dlookr::plot_correlate(carseats, Sales, Price)
# carseats %>%
#   dplyr::filter(ShelveLoc == "Good") %>%
#   dplyr::group_by(Urban, US) %>%
#   dlookr::plot_correlate(Sales)



## Summary statistics by – overall with correlation
# SmartEDA::ExpNumStat(
#   Carseats,
#   by = "A",
#   gp = "Price",
#   Qnt = seq(0, 1, 0.1),
#   MesofShape = 1,
#   Outlier = TRUE,
#   round = 2
# )




# https://alastairrushworth.github.io/inspectdf/articles/pkgdown/inspect_cor_exampes.html
# inspectdf::inspect_cor(storms)

# inspectdf::inspect_cor(storms) %>% inspectdf::show_plot()

# inspectdf::inspect_cor(storms, storms[-c(1:200), ])

# inspectdf::inspect_cor(storms, storms[-c(1:200), ]) %>%
#   slice(1:20) %>%
#   inspectdf::show_plot()




# https://neuropsychology.github.io/psycho.R/2018/05/20/correlation.html

# devtools::install_github("neuropsychology/psycho.R")


# library(psycho)

# cor <- psycho::affective %>%
# correlation()

# summary(cor)


# plot(cor)


# print(cor)






# cor %>%
#   report::to_values()

# summary(cor) %>%
#   knitr::kable(format = "latex") %>%
#   kableExtra::kable_styling(latex_options="scale_down")


# ggplot(mydata, aes(x = tx_zamani_verici_yasi, y = trombosit)) +
#   geom_point() +
#   geom_smooth(method = lm, size = 1)



# References:
#   Automated Interpretation of Metrics and Effect Sizes
#   https://easystats.github.io/report/articles/interpret_metrics.html




# mydata %>%
#   select(continiousVariables,
#  -dateVariables) %>%
#   visdat::vis_cor()



