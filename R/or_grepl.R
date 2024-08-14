#' Returns the OR-ed result from applying a list of patterns on string x 
#' 
#' @param patterns 
#' @param x 
#' @param ignore.case 
#' @param perl 
#' @param fixed 
#' @param useBytes 
#' @return A boolean
#' @author Guilherme V Rocha
#' @export
or_grepl = function(patterns, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE){
  return(apply(sapply(patterns, 
                      function(p){
                        grepl(p, 
                              x, 
                              ignore.case = ignore.case,
                              perl = perl,
                              fixed = fixed,
                              useBytes = useBytes)
                      }), 
               1, 
               function(y){return(any(y))}))
}
