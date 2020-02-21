#' Makes the field notes column to lower case
#'
#' @param ds dataframe - a ubc herbarium spreadsheet
#'
#' @return dataframe
#' @export
#'
#' @examples
lower <- function(ds){
  split <- stringr::str_split(ds$`Field Notes`," ",n = 2)

  cap <- purrr::map(split,~stringr::str_to_lower(.x[1]))

  ds$`Field Notes` <- purrr::map2(split,cap,~c(.y,.x[2]) %>%
                             paste(.,collapse = " "))

  ds$`Field Notes` <- gsub("NA","",ds$`Field Notes`)

  return(ds)
}
