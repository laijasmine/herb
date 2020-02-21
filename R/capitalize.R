#' Makes the first word in Habitat lowercase
#'
#' @param ds dataframe
#'
#' @return dataframe
#' @export
#'
#' @examples
capitalize <- function(ds){
  split <- stringr::str_split(ds$Habitat," ",n = 2)

  cap <- purrr::map(split,~stringr::str_to_sentence(.x[1]))

  ds$Habitat <- purrr::map2(split,cap,~c(.y,.x[2]))

  ds$Habitat <- paste(ds$Habitat,collapse = " ")

  ds$Habitat <- gsub("NA","",ds$Habitat)

  return(ds)
}
