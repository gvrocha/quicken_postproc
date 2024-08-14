#' Returns the OR-ed result from applying a list of patterns on string x 
#' 
#' @param patterns the patterns over which to execute or on
#' @param x the string on which to search for patterns
#' @param ignore.case as in grepl
#' @param perl as in grepl
#' @param fixed as in grepl
#' @param useBytes as in grepl
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
