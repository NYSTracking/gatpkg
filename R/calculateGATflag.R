#'calculateGATflag
#'
#'
#'@description This function calculates which rows should be excluded by
#'             assigning a GATflag value of 1. The rows are chosen based
#'             on the exclusion criteria contained in exclist.
#'
#'@param exclist The settings to define areas to be excluded.
#'@param d The dataset containing the flag and exclusion variables.
#'
#'
#'
#'
#'@export



calculateGATflag <- function(exclist, d) {
  if (!"GATflag" %in% names(d)) {
    d$GATflag <- 0 # for non-default uses of this function
  }
  d$GATflag <- ifelse(d$GATflag == 1, 0, d$GATflag)
  
  for(i in seq(1, 9, by = 3)){
    if (exclist[[i]] != "NONE") {
      
      
      if (exclist[[i + 1]] == "equals") {
        d$GATflag <- ifelse(d$GATflag == 0 & data.frame(d)[, exclist[[i]]] == exclist[i + 2], 1, d$GATflag)
      } else if (exclist[[i + 1]] == "less than") {
        d$GATflag <- ifelse(d$GATflag == 0 & data.frame(d)[, exclist[[i]]] < exclist[i + 2], 1, d$GATflag)
      } else if (exclist[[i + 1]] == "greater than") {
        d$GATflag <- ifelse(d$GATflag == 0 & data.frame(d)[, exclist[[i]]] > exclist[i + 2], 1, d$GATflag)
      }
      
    }
  }
  return(d$GATflag)
} 
