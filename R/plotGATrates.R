#'plotGATrates
#'
#'
#'@description This function takes exclist and ratevars and produces a plot of the chosen rates,
#'             if selected.
#'
#'@param ratevars Rate settings, if pre-defined.
#'@param myshps a list containing the shapefiles
#'@param exclist List of exclusion criteria to use when merging.
#'
#'@param closemap   A boolean to denote whether to close the map windows after
#'                   the maps are drawn and saved. The default setting is TRUE
#'                   and will result in maps remaining open after GAT is
#'                   finished running. Maps are saved to PDF irrespective of
#'                   this setting.
#'
#'
#'@export



plotGATrate = function(ratevars, myshps, closemap, exclist){

  #function to handle numbers
  numformat <- function(num) {
    format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
  }
  
  gats <- list(title = paste(ratevars$ratename, "per",
                             format(as.numeric(ratevars$multiplier),
                                    big.mark = ",", scientific=FALSE),
                             "after merging"),
               sub = paste("Rate calculation:", ratevars$ratename, "=",
                           format(as.numeric(ratevars$multiplier),
                                  big.mark = ",", scientific=FALSE), "*",
                           ratevars$numerator, "/", ratevars$denominator))
  
  
  for(i in seq(1, 9, by = 3)){
    if(exclist[i] != "NONE"){
      textseparation <- if(i == 1) "\nExclusion criteria: " else ";"
      
      gats$sub <- paste(gats$sub, textseparation,
                        exclist[i], exclist[i + 1],
                        numformat(exclist[i + 2]))
    }
    
  }
  
  
  rateplot <- plotGATmaps(area = myshps$aggregated,var = ratevars$ratename,
                          clr = ratevars$colorscheme,
                          title.sub = gats$sub,
                          title.main = gats$title,
                          ratemap = TRUE,
                          mapstats = TRUE,
                          closemap = closemap)
  
  return(rateplot)
}
