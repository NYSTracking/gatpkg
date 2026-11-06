#'drawGATpb
#'
#'@description This function creates and updates the GAT progress bar title and label
#'             base on the value of step. The titles and labels are stored in a df
#'             called PBnames with row numbers equivalent to step + 1.
#'
#'@param gatenv The environment in gat which loads progressbars.
#'@param step Integer step in the GAT program, for help reference.
#'@param mysettings List of system settings, including version, pkgdate, starttime, and the booleans savekml and exists.
#'@param filevars List of file names and paths. Of relevance to this function are the filename, filein, and the combined savepath and save name, userout.
#'@param aggvars A list of objects produced by the merging process.
#'@param ratevars A list of objects needed to calculate rates. It contains the strings ratename, numerator, and denominator and the number multiplier.
#'
#'@examples
#'#'#GAT progress bar for step 0:
#'drawGATpb(gatenv = new.env(), step = 1, mysettings = NULL, filevars = NULL,
#'          aggvars = NULL, ratevars = NULL)
#'
#'GAT progress bar for step 1:
#'#'drawGATpb(gatenv = new.env(), step = 1, mysettings = NULL, filevars = NULL,
#'          aggvars = NULL, ratevars = NULL)
#'
#'@export



#In runGATprogram, the first pb drawing begins at zero.
#While the rownames could have been renamed, setting the title and label
#to step + 1 seemed easier both to implement and understand/read.


drawGATpb <- function(gatenv, step, mysettings, filevars, aggvars, ratevars) {
  gatenv$pb$title <- eval(parse(text = PBnames$PBtitles[step + 1]))
  
  gatenv$pb$label <- eval(parse(text = PBnames$PBlabels[step + 1]))
  
  
  tryCatch(tcltk::setTkProgressBar(gatenv$tpb, title = gatenv$pb$title,
                                   value = step, label = gatenv$pb$label),
           error = function(e) gatenv$tpb <- NULL)
  
  if (is.null(gatenv$tpb)) {
    gatenv$tpb <- tcltk::tkProgressBar(title = gatenv$pb$title, initial = 0,
                                       label = gatenv$pb$label, min = 0,
                                       max = 26, width = 400)
    tcltk::setTkProgressBar(gatenv$tpb, value = step)
  }
}
  



















