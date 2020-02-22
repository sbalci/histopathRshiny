# time interval calculation function
#' Title
#'
#' @param mydata 
#' 
#' @importFrom magrittr %>% 
#' 
#' @param SurgeryDate 
#' @param LastFollowUpDate 
#'
#' @return
#' @export
#'
time_interval <- function(mydata, SurgeryDate, LastFollowUpDate) {
   mydata %>%
    dplyr::mutate(
      interval = lubridate::interval(
      lubridate::ymd(SurgeryDate),
      lubridate::ymd(LastFollowUpDate)
    )
    )
}
