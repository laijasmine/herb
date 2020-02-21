#' Originally Identified As
#'
#' given the name and determiner and year and formats it into the Original ID as format
#' note must use group_by on a unique identifier when using mutate
#'
#' @param g genus
#' @param q quantifier
#' @param s species
#' @param sub subspecies
#' @param v variety
#' @param f forma
#' @param det determined by
#' @param yr year
#'
#' @return Character
#' @export
#'
#' @examples oia("gen","q","s","sub","var","for","det","yr")
oia <- function(g,q,s,sub,v,f,det,yr){
  taxonomy <- c(g,q,s)
  id <- c()
  for(name in taxonomy){
    if(!is.na(name)){
      id <- append(id,name)
    }
  }

  prefix <- c("subsp.","var.","f.")
  lower <- c(sub,v,f)

  if(!is.na(sub)){
    id <- c(id,"subsp.",sub)
  }
  if(!is.na(v)){
    id <- c(id,"var.",v)
  }
  if(!is.na(f)){
    id <- c(id,"f.",f)
  }

  return(paste(c(id,"by",det,yr), collapse = " "))}
