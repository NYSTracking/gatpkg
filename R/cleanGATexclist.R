#'cleanGATexclist
#'
#'@description This function takes the inputs for exclist and standardizes 
#'             the numeric portion so that no commas are present. The values
#'             of i are equivalent to:
#'             i = var
#'             i + 1 = math
#'             i + 2 = val
#'             Where i denotes which exclist the variables are taken from among
#'             1, 2, and 3.
#'
#'@param step Integer step in the GAT program, for help reference.
#'@param exclist The settings to define areas to be excluded.
#'@param temp an environment storing GAT information like the flagconfirm boolean.
#'@param backopt Boolean denoting whether to include the back button.
#'@param quitopt Text string for the cancel button.
#'@param bgcol Text string containing UI background color.
#'@param buttoncol Text string containing UI button color.
#'
#'@examples
#'
#'#Set some conditions
#'TESTexclist = list(var1 = "VAR1", math1 = "less than", Val1 = "6,500",
#'               var2 = "VAR2", math2 = "greater than", Val2 = "550,000",
#'               var3 = "VAR3", math3 = "equals", Val3 = "0")
#'TESTtemp = list(flagconfirm = FALSE)
#'
#'#Removes "," from inputs
#'cleanGATexclist(step = 5, exclist = TESTexclist, temp = TESTtemp, quitopt = "Cancel GAT", 
#'                bgcol = "lightskyblue3", buttoncol = "cornflowerblue")
#'                
#'
#'@export



cleanGATexclist <- function(step, exclist, temp, quitopt, bgcol, buttoncol){

  for(i in seq(1, 9, by = 3)){  
    while (grepl("[^0-9.,-]", exclist[i + 2])) {
      if (exclist[i + 1] == "less than") {
        temp$minmaxlbl <- "maximum"
      } else if (exclist[i + 1] == "greater than") {
        temp$minmaxlbl <- "minimum"
      } else {
        temp$minmaxlbl <- ""
      }
      gats <- list(title = paste("Threshold for", exclist[i]),
                   msg = paste("Please enter a valid", temp$minmaxlbl, "number for",
                               exclist[i]),
                   help = paste0("To continue, enter a valid number and click 'Next >',",
                                 "\nto return to exclusion selection, click '< Back',",
                                 "\nand to quit the program, click '", quitopt, "'."))
      exclist[i + 2] <- inputGATvalue(title = gats$title, help = gats$help,
                                    message = gats$msg, defaulttext = "1,000",
                                    helppage = "inputGATvalue", step = step,
                                    backopt = !temp$flagconfirm,
                                    bgcol = bgcol, quitopt = quitopt,
                                    buttoncol = buttoncol)
      if (exclist[i + 2] == "back") {
        exclist$var1 <- "back"
        exclist[i + 2] <- 0
      } else if (exclist[i + 2] == "cancel") {
        exclist$var1 <- "cancel"
        exclist[i + 2] <- 0
      }
    }
    exclist[i  + 2] <- as.numeric(gsub(",", "", exclist[i  + 2]))
  }
  return(exclist)
}
