#' Directions abbreviated
#'changes the N E W S  to full words in the dataframe
#' @param df dataframe takes in a herbarium spreadsheet
#'
#' @return dataframe
#' @export
#'
#' @examples
dir_abb <- function(df){
  df_loc <- dplyr::mutate(df,
                  Location = stringr::str_replace_all(df$Location,"NNW(?=[:blank:])|NNW-","north-northwest"),
                  Location = stringr::str_replace_all(df$Location,"NNE(?=[:blank:])|NNE-","north-northeast"),
                  Location = stringr::str_replace_all(df$Location,"SSE(?=[:blank:])|SSE-","south-southeast"),
                  Location = stringr::str_replace_all(df$Location,"SSW(?=[:blank:])|SSW-","south-southwest"),
                  Location = stringr::str_replace_all(df$Location,"WNW(?=[:blank:])|WNW-","west-northwest"),
                  Location = stringr::str_replace_all(df$Location,"SW(?=[:blank:])|SW-","southwest"),
                  Location = stringr::str_replace_all(df$Location,"NW-","northwest-"),
                  Location = stringr::str_replace_all(df$Location,"NW(?=[:blank:])","northwest"),
                  Location = stringr::str_replace_all(df$Location,"SE(?=[:blank:])|SE-","southeast"),
                  Location = stringr::str_replace_all(df$Location,"NE(?=[:blank:])|NE-","northeast"),
                  Location = stringr::str_replace_all(df$Location,"S(?=[:blank:])|S-","south"),
                  Location = stringr::str_replace_all(df$Location,"W(?=[:blank:])|W-","west"),
                  Location = stringr::str_replace_all(df$Location,"\\(W\\)","\\(west\\)"),
                  Location = stringr::str_replace_all(df$Location,"N(?=[:blank:])|N-|N/","north"),
                  Location = stringr::str_replace_all(df$Location,"E(?=[:blank:])|E-","east"))

  return(df_loc)}
