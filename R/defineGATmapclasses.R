#' Define Classes for Map Legends
#'
#' @description
#' This function prepares the classes for the original and aggregated maps
#' so that the same scale can be used for both.
#'
#' @param areaold A spatial polygons data frame.
#' @param areanew A second spatial polygons data frame that ahould have the
#'                same outer boundary as the first one.
#' @param aggvar  The variable to be mapped.
#' @param breaks  The number of desired categories in the maps.
#'               
#' @examples
#' # prepare the classes for the choropleth maps
#' defineGATmapclasses(
#'   areaold = hftown,
#'   areanew = hfagg610k,
#'   aggvar = "TOTAL_POP",
#'   breaks = 4
#' )
#'
#' @export



defineGATmapclasses <- function(areaold, areanew, aggvar, breaks = 7) {
  olddata <- as.numeric(data.frame(areaold)[, aggvar])
  newdata <- as.numeric(data.frame(areanew)[, aggvar])
  combinedata <- c(olddata, newdata)
  
  if (length(table(combinedata)) < breaks) {
    breaks <- length(table(combinedata))
  }
  
  if (breaks > 2) {
    plotclr <- RColorBrewer::brewer.pal(breaks, "OrRd")
  } else if (breaks == 2) {
    plotclr <- RColorBrewer::brewer.pal(3, "OrRd")[c(1, 3)]
  } else if (breaks == 1) {
    plotclr <- RColorBrewer::brewer.pal(3, "OrRd")[1]
  }
  
  class <- classInt::classIntervals(combinedata, breaks, style = "quantile")
  if (breaks < 4) {
    class$brks <- c(floor(class$brks[1]*10)/10,
                     round(class$brks[2:breaks], digits = 1),
                     ceiling(class$brks[(breaks + 1)]*10)/10)
  } else {
    class$brks <- c(floor(class$brks[1]), round(class$brks[2:breaks]),
                     ceiling(class$brks[(breaks + 1)]))
  }
  
  classbefore <- classInt::classIntervals(olddata, n = breaks,
                                           style = "fixed", fixedBreaks = class$brks)
  colcodebefore <- classInt::findColours(classbefore, plotclr)
  
  tempbrks <- c(floor(min(newdata)), class$brks[class$brks > min(newdata)])
  
  classafter <- classInt::classIntervals(newdata, n = breaks,
                                          style = "fixed", fixedBreaks = tempbrks)
  classafter$brks <- class$brks
  colcodeafter <- classInt::findColours(classafter, plotclr)
  

  myvars <- list(colcodebefore, colcodeafter)
  
  
  return(myvars)
}

