#' Return a list of categories given a data.frame
#'
#'
#' @return named list with each entry listing all categories within a variable in the df
#' @export
#'
get_category_variables <- function(df) {
  
  ## get variable classes
  varClass <- sapply(df, class)
  
  ## grab characters - they may be categorical, or unique ids
  characterVars <- names(varClass[varClass %in% c("character", "factor")])
  
  #remove those character variables that aren't 
  char_list <- sapply(characterVars, function(x){
    size_vec <- length(df[[x]])
    cate_vec <- length(unique(df[[x]]))
    ##This is a bit o a kludge - don't want to return categories which 
    ##have levels bigger than nrow(df)/2
    if(cate_vec > (size_vec/2)){
      out <- NULL
    } else{
      out <- as.character(unique(df[[x]]))
    }
    return(out)
  })
  
  ##remove null values from list
  char_list <- char_list[lapply(char_list,length)!=0]
  
  return(char_list)
  
}
