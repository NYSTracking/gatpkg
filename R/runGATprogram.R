#' Run the Geographic Aggregation Tool (GAT)
#'
#' @description
#'
#' This function runs GAT and does not require any inputs. The pop-up dialogs
#' it creates guide the user to define all parameters. For step by step details
#' on how GAT works, browse the package vignettes, especially the GAT manual,
#' which you can access via
#' \href{../doc/gat_tutorial.html}{
#' \code{vignette("gat_tutorial", package = "gatpkg")}}.
#'
#' @param limitdenom A boolean denoting whether to force denominators in rates
#'                   and merge ratios to contain only non-zero values.
#' @param pwrepeat   A boolean denoting whether population weighting (if used)
#'                   should be recalculated each time two areas are merged
#'                   (TRUE) or if area centroids should be weighted with area
#'                   populations (FALSE). If population weighting is not used,
#'                   this option is ignored. If population weighting is used,
#'                   a polygon shapefile containing population is needed.
#' @param settings   The filepath to an Rdata file previously created by GAT.
#' @param adjacent   A boolean denoting whether to force GAT to merge only
#'                   adjacent areas.
#' @param minfirst   A boolean denoting whether or not to select the most
#'                   desirable neighbor only from among the neighbors that
#'                   have values below the desired minimum. If no neighbors
#'                   are below the desired minimum, the most desirable of all
#'                   eligible neighbors is selected.
#' @param closemap   A boolean to denote whether to close the map windows after
#'                   the maps are drawn and saved. The default setting is TRUE
#'                   and will result in maps remaining open after GAT is
#'                   finished running. Maps are saved to PDF irrespective of
#'                   this setting.
#' @param quitopt    Text string for the cancel button.
#' @param bgcol      Text string containing UI background color.
#' @param buttoncol  Text string containing UI button color.
#'
#' @details
#' For more information  on how the different merge options work, see
#' "How GAT identifies areas to merge" in the
#' \href{../doc/gat_tech_notes.html}{
#' \code{vignette("gat_tech_notes", package = "gatpkg")}}.
#'
#' @examples
#'
#' if (interactive()) {
#' # this code will run the default version of GAT
#' runGATprogram()
#' }
#'
#' @export

# limitdenom = FALSE; pwrepeat = FALSE; settings = NULL
# adjacent = TRUE; minfirst = TRUE; closemap = TRUE
# bgcol = "lightskyblue3"; buttoncol = "cornflowerblue"; quitopt = "Cancel GAT"

runGATprogram <- function(limitdenom = FALSE, pwrepeat = FALSE, settings = NULL,
                          adjacent = TRUE, minfirst = FALSE, closemap = FALSE,
                          bgcol = "lightskyblue3", buttoncol = "cornflowerblue",
                          quitopt = "Cancel GAT") {
  #  1. start the GAT program ----
  gatenv <- new.env()

  mysettings <- list(version = utils::packageDescription("gatpkg")$Version,
                     pkgdate = utils::packageDescription("gatpkg")$Date,
                     adjacent = adjacent, pwrepeat = pwrepeat,
                     minfirst = minfirst, limitdenom = limitdenom,
                     starttime = Sys.time(), quit = FALSE)

  # load the progress bar
  # note https://stackoverflow.com/questions/8436045/double-r-tcltk-progress-bar
  step <- 0
  drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = NULL,
            aggvars = NULL, ratevars = NULL)

  # pre-load lists ----
  step <- 1 # start at step 1
  temp <- list(flagconfirm = FALSE, msg = "")
  myshps <- list()

  if (!is.null(settings)) {
    load(settings)
    step <- 11
    temp$flagconfirm <- TRUE
    filevars$userout <- paste0(filevars$userout, "_2")
    filevars$fileout <- paste0(filevars$fileout, "_2")
    temp$shp <- sf::st_read(dsn = filevars$pathin,
                            layer = filevars$filein)
    temp$numerics <- checkGATvariabletypes(temp$shp, type = "number")
    temp$old_vars <- c()
    for (i in 1:ncol(temp$shp)) {
      # possibly these can be removed entirely, but need to verify
      # if removing, inform user with warning dialog
      # not sure how to handle flag variable - rename it?
      if (names(temp$shp)[i] %in%
          c("old_GATx", "old_GATy", "old_GATnumIDs", "old_GATcratio",
            "old_flag", "old_GATpop")) {
        temp$old_vars <- c(temp$old_vars, names(temp$shp)[i])
      }
    }
    if (length(temp$old_vars) > 0) {
      temp$msg <- paste(temp$msg,
                        paste(temp$old_vars, collapse = ", "),
                        "were removed from the data frame.")
    }
    temp$shp <- temp$shp[,names(temp$shp)[!names(temp$shp) %in% temp$old_vars]]
    for (i in 1:ncol(temp$shp)) {
      if (names(temp$shp)[i] %in%
          c("GATx", "GATy", "GATnumIDs", "GATcratio", "GATflag", "GATpop")) {
        temp$msg <- paste(temp$msg,
                          "\n", names(temp$shp)[i], "has been changed to",
                          paste0("old_", names(temp$shp)[i], "."))
        names(temp$shp)[i] <- paste0("old_", names(temp$shp)[i])
      }
    }
    if (!temp$msg == "") {
      tcltk::tkmessageBox(title = "Some variable names changed", type = "ok",
                          icon = "warning", message = temp$msg)
    }
    if (!"GATflag" %in% names(temp$shp)) {
      temp$shp$GATflag <- 0
    }
    temp$mapflag <- temp$shp[temp$shp$GATflag == 0, ] # not needed?
    if (gatvars$popwt) {
      temp$pop <- sf::st_read(dsn = filevars$poppath,
                              layer = filevars$popfile)
    }
    temp$nrow <- nrow(temp$shp)
    temp$shp <- temp$shp[!sf::st_is_empty(temp$shp),]
    temp$invalid <- nrow(temp$shp) - temp$nrow

    if (temp$invalid > 0) {
      temp$msg <- paste("Please note", temp$invalid,
                        "areas were removed because they were empty.")
      tcltk::tkmessageBox(title = "Areas invalid", type = "ok",
                          icon = "warning", message = temp$msg)
    }

  } else {
    gatvars <- list(invalid = temp$invalid)
    filevars <- list(userin = "")
    mergevars <- NULL
    ratevars <- NULL
    exclist <- NULL
    aggvars <- NULL
  }

  # pre-load number function ----
  numformat <- function(num) {
    format(as.numeric(gsub(",", "", num)), big.mark=",", scientific=FALSE)
  }


  #  2. start user input ####

  while (step < 20){ # user input settings
    while (step ==  1) { # ask user to identify shapefile; check its usability
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      # identify shapefile
      filevars <- locateGATshapefile(myfile = filevars$userin, step = step,
                                     msg = "Select the shapefile to aggregate",
                                     bgcol = bgcol, buttoncol = buttoncol)

      if (filevars$userin == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else {
        temp$shp <- sf::st_read(dsn = filevars$pathin,
                                layer = filevars$filein)

        # check for both numeric and character data
        temp$numerics <- checkGATvariabletypes(temp$shp, type = "number")
        temp$alphas <- checkGATvariabletypes(temp$shp, type = "character")
        temp$polys <- sum(grepl("POLYGON", class(temp$shp$geometry),
                                fixed = TRUE)) > 0
        # add dialog specifying each issue
        # add check for shapefile type (if class(shp) )
        temp$msg <- ""
        temp$error <- FALSE
        if (nrow(temp$shp) < 2) {
          temp$issue <- "at least 2 areas."
          temp$error <- TRUE
        } else if (!temp$polys) {
          # message: wrong kind of shapefile; repeat dialog
          temp$issue <- "polygons."
          temp$error <- TRUE
        } else if (length(temp$numerics) == 0) {
          # message: there are no numeric variables; repeat dialog
          temp$issue <- "any numeric variables."
          temp$error <- TRUE
        } else if (length(temp$alphas) == 0) {
          # message: there are no character variables; repeat dialog
          temp$issue <- "any non-numeric variables."
          temp$error <- TRUE
        } else {
          temp$old_vars <- c()
          for (i in 1:ncol(temp$shp)) {
            # possibly these can be removed entirely, but need to verify
            # if removing, inform user with warning dialog
            # not sure how to handle flag variable - rename it?
            if (names(temp$shp)[i] %in%
                c("old_GATx", "old_GATy", "old_GATnumIDs", "old_GATcratio",
                  "old_flag", "old_GATpop")) {
              temp$old_vars <- c(temp$old_vars, names(temp$shp)[i])
            }
          }
          if (length(temp$old_vars) > 0) {
            temp$msg <- paste(temp$msg,
                              paste(temp$old_vars, collapse = ", "),
                              "were removed from the data frame.")
          }
          temp$shp <- temp$shp[, !names(temp$shp) %in% temp$old_vars]
          for (i in 1:ncol(temp$shp)) {
            if (names(temp$shp)[i] %in% c("GATx", "GATy", "GATnumIDs",
                                          "GATcratio", "GATflag", "GATpop")) {
              temp$msg <- paste(temp$msg, "\n", names(temp$shp)[i],
                                "has been changed to", paste0("old_",
                                substr(names(temp$shp)[i], 1, 6), "."))
              names(temp$shp)[i] <- paste0("old_", substr(names(temp$shp)[i],
                                                          1, 6))
            }
          }
          if (temp$msg != "") {
            tcltk::tkmessageBox(title = "Some variable names changed",
                                type = "ok", icon = "warning",
                                message = temp$msg)
          }

          temp$shp$GATflag <- 0
          temp$nrow <- nrow(temp$shp)
          temp$shp <- temp$shp[!sf::st_is_empty(temp$shp),]
          gatvars$invalid <- temp$nrow - nrow(temp$shp)

          if (gatvars$invalid > 0) {
            temp$msg <- paste("Please note", gatvars$invalid,
                              "areas were removed because they were invalid.")
            tcltk::tkmessageBox(title = "Areas invalid", type = "ok",
                                icon = "warning", message = temp$msg)
          }

          step <- step + 1
        }
        if (temp$error) {
          temp$msg <- paste("The shapefile", filevars$filein,
                            "does not contain", temp$issue, "\n",
                            "Please select a new shapefile.")
          tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                              icon = "error", message = temp$msg)
          temp$error <- FALSE
        }
      }

    } # shapefile
    while (step ==  2) { # ask user to select ID variable that uniquely
                        # identifies areas to be merged
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      # identify GAT polygon identifier variable
      gatvars$myidvar <- identifyGATid(shp = temp$shp, step = step,
                                       backopt = !temp$flagconfirm,
                                       bgcol = bgcol, quitopt = quitopt,
                                       buttoncol = buttoncol)

      if (gatvars$myidvar == "back"){
        step <- step - 1
      } else if (gatvars$myidvar == "cancel") {
        mysettings$quit <- TRUE
        step <- 20
      } else if (gatvars$myidvar == "missing") {
        temp$msg <- paste("The shapefile does not contain a suitable ID variable.",
                          "A temporary ID will be created.")
        tcltk::tkmessageBox(title = "No suitable ID variable", message = temp$msg,
                            type = "ok", icon = "warning")
      } else { # go ahead
        temp$shp$GATid <- data.frame(temp$shp)[, gatvars$myidvar]
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
    } # id variable
    while (step ==  3) { #ask user to select boundary variable, if present
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      if (is.null(gatvars$boundary)) {
        gatvars$boundary <- "NONE"
        gatvars$rigidbound <- FALSE
      }

      tempbound <- NULL

      while (is.null(tempbound)) {
        tempbound <- identifyGATboundary(shp = temp$shp, step = step,
                                         boundary = gatvars$boundary,
                                         borders = gatvars$rigidbound,
                                         backopt = !temp$flagconfirm,
                                         bgcol = bgcol, quitopt = quitopt,
                                         buttoncol = buttoncol)
      }

      gatvars$rigidbound <- tempbound$check
      gatvars$boundary <- tempbound$myvar


      if (gatvars$boundary == "NONE") {
        gatvars$rigidbound <- FALSE # just in case user selects both
      }

      if (gatvars$boundary == "back") {
        step <- step - 1
      } else if (gatvars$boundary == "cancel") {
        mysettings$quit <- TRUE
        step <- 20
      } else {
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
      rm(tempbound)
    } # boundary variable
    while (step ==  4) { # ask for aggregation variables
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      if (is.null(gatvars$aggregator1)) {
        agglist <- NULL
      } else {
        agglist <- list(var1 = gatvars$aggregator1,
                        var2 = gatvars$aggregator2,
                        minval1 = gatvars$minvalue1,
                        maxval1 = gatvars$maxvalue1,
                        minval2 = gatvars$minvalue2,
                        maxval2 = gatvars$maxvalue2)
      }
      temp$error <- TRUE

      # re-call the function as needed
      while (temp$error) {
        agglist <- identifyGATaggregators(shp = temp$shp, step = step,
                                          agglist = agglist,
                                          backopt = !temp$flagconfirm,
                                          bgcol = bgcol, quitopt = quitopt,
                                          buttoncol = buttoncol)
        temp$error <- FALSE
        if (is.null(agglist)) {
          x <- confirmGATquit()
          if (x == "quit") {
            agglist <- list(var1 = "cancel")
          } else {
            temp$error <- TRUE
            agglist <- list(var1 = "")
          }
        } else if (length(agglist$var1) == 0) {
          temp$error <- TRUE
          agglist <- NULL
        }
      }

      if (agglist$var1 == "back") {
        step <- step - 1
      } else if (agglist$var1 == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else if (!temp$error) {
        gatvars$aggregator1 <- agglist$var1
        gatvars$minvalue1 <- agglist$minval1
        gatvars$maxvalue1 <- agglist$maxval1
        if (agglist$var2 == "NONE") {
          gatvars$aggregator2 <- agglist$var1
          gatvars$minvalue2 <- agglist$minval1
          gatvars$maxvalue2 <- agglist$maxval1
        } else {
          gatvars$aggregator2 <- agglist$var2
          gatvars$minvalue2 <- agglist$minval2
          gatvars$maxvalue2 <- agglist$maxval2
        }
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
      gatvars$ismax1 <- if (agglist$maxval1 ==
                            sum(data.frame(temp$shp)[, agglist$var1],
                                na.rm = TRUE)) TRUE else FALSE
      if (!agglist$var2 == "NONE") {
        gatvars$ismax2 <- if (agglist$maxval2 ==
                              sum(data.frame(temp$shp)[, agglist$var2],
                                  na.rm = TRUE)) TRUE else FALSE
        gatvars$ismin2 <- if (agglist$minval2 ==
                              min(data.frame(temp$shp)[, agglist$var2],
                                   na.rm = TRUE)) TRUE else FALSE
      }
      rm(agglist)

      temp$shp$GATflag <- ifelse(data.frame(temp$shp)[, gatvars$aggregator1] >
                                   as.numeric(gsub(",", "", gatvars$maxvalue1)) |
                                 data.frame(temp$shp)[, gatvars$aggregator2] >
                                   as.numeric(gsub(",", "", gatvars$maxvalue2)),
                                 5, temp$shp$GATflag)
      gatvars$exclmaxval <- sum(temp$shp$GATflag == 5)
    } # aggregation variables
    while (step ==  5) {
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)
      
      if (!exists("exclist")) exclist <- NULL
      temp$error <- TRUE
      
      while (temp$error) {
        exclist <- inputGATexclusions(shp = temp$shp, exclist = exclist,
                                      step = step, backopt = !temp$flagconfirm,
                                      bgcol = bgcol, quitopt = quitopt,
                                      buttoncol = buttoncol)
        temp$error <- FALSE
        if (is.null(exclist)) {
          x <- confirmGATquit()
          if (x == "quit") {
            exclist <- list(var1 = "cancel")
          } else {
            temp$error <- TRUE
            exclist <- list(var1 = "")
          }
        }
        
        # get rid of all non-numeric characters
        # move this to a function?
        if (!temp$error & !exclist$var1 %in% c("back", "cancel")) {
          
          exclist <- cleanGATexclist(step = step, exclist = exclist, temp = temp, quitopt = quitopt, bgcol = bgcol, buttoncol = buttoncol)
          
        } else if (exclist$var1 == "back") {
          step <- step - 1
        } else if (exclist$var1 == "cancel") {
          mysettings$quit <- TRUE
          step <- 20
        }
      }
      
      if (!exclist$var1 %in% c("back", "cancel", "repeat")) {
        # calculate exclusions now to use them to calculate the
        # denominator for similar merges and rates
        temp$shp$GATflag <- calculateGATflag(exclist, d = temp$shp)
        exclist$flagsum <- sum(temp$shp$GATflag != 0)
        
        if (nrow(temp$shp) - exclist$flagsum < 2) {
          temp$msg <- paste("This selection will exclude", exclist$flagsum,
                            "of", nrow(temp$shp), "areas.", "\n",
                            "GAT requires at least 2 areas to run.", "\n",
                            "Please select new exclusion criteria.")
          tcltk::tkmessageBox(title = "Selections invalid", type = "ok",
                              icon = "error", message = temp$msg)
          temp$error <- TRUE
        } else if (exclist$var1 != "NONE") {
          temp$msg <- "You have selected to exclude:"
          i <- list(var = c(exclist$var1, exclist$var2, exclist$var3),
                    math = c(exclist$math1, exclist$math2, exclist$math3),
                    val = c(exclist$val1, exclist$val2, exclist$val3))
          for (j in 1:3) {
            if (i$var[j] != "NONE") {
              temp$msg <- paste(temp$msg, "\n   ", i$var[j], i$math[j], i$val[j])
            }
          }
          
          temp$msg <- paste(temp$msg, "\nThis will exclude", exclist$flagsum,
                            "of", nrow(temp$shp), "areas.")
          temp$help <- paste0("To continue, select 'Yes',",
                              "\nto reselect exclusion criteria, select 'Repeat',",
                              "\nand to return to second aggregation variable selection,",
                              "click '< Back',")
          
          temp$cancel <- inputGATmessage(title = "Excluded areas", help = temp$help,
                                         helptitle = "inputGATmessage",
                                         helppage = "inputGATmessage", step = 6,
                                         msg = temp$msg, quitopt = "Repeat",
                                         backopt = FALSE, bgcol = bgcol,
                                         buttoncol = buttoncol)
          if (temp$cancel == "cancel") {
            exclist$var1 <- "repeat"
          } else if (temp$cancel == "back") {
            exclist$var1 <- "back"
            step <- step - 1
          }
        }
        
      }
      
      if (!exclist$var1 %in% c("back", "cancel", "repeat") & !temp$error) {
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      } else if (exclist$var1 == "back") {
        step <- step - 1
      } else if (exclist$var1 == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else if (exclist$var1 == "repeat") {
        temp$error <- TRUE
      }
      
    } # exclusions (exclist)
    while (step ==  6) { # radiobutton dialog to get merge type
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      # create a non-flagged subset of the main dataset to determine denominators
      temp$mapflag <- temp$shp[temp$shp$GATflag == 0, ]

      if (!exists("mergevars")) mergevars <- NULL
      if (is.null(mergevars$centroid)) mergevars <- NULL
      if (!exists("limitdenom")) limitdenom <- FALSE
      temp$error <- TRUE

      while (temp$error) {
        mergevars <- inputGATmerge(shp = temp$mapflag, mergevars = mergevars,
                                   aggvar = gatvars$aggregator1,
                                   aggvar2 = gatvars$aggregator2, step = step,
                                   limitdenom = limitdenom, bgcol = bgcol,
                                   backopt = !temp$flagconfirm,
                                   quitopt = quitopt, buttoncol = buttoncol)
        temp$error <- FALSE
        if (is.null(mergevars)) {
          x <- confirmGATquit()
          if (x == "quit") {
            mergevars <- list(similar1 = "cancel")
          } else {
            mergevars <- list(similar1 = "repeat", mergeopt1 = 0)
            temp$error <- TRUE
          }
        }

        if (mergevars$similar1 == "back") {
          step <- step - 1 # back to exclusions
        } else if (mergevars$similar1 == "cancel") {
          mysettings$quit <- TRUE
          step <- 20
        } else if (mergevars$similar1 != "repeat" & mergevars$mergeopt1 != "0") {
          # quality control for similarity: force selection of different numerator
          # and denominator variables and check for bad data
          if (mergevars$mergeopt1 != "similar") {
            step = step + 1
          } else {
            if (mergevars$similar1 == mergevars$similar2) {
              temp$msg <- "Please select different variables for numerator and denominator."
              tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                  type = "ok", icon = "warning")
              temp$error <- TRUE # re-run step
            } else {
              temp$go <- TRUE # passed first check
              # if similarvar2 has zero in denominator, require user to pick again
              # this should no longer be an issue
              temp$check <- is.finite(data.frame(temp$mapflag)[ , mergevars$similar1] /
                                      data.frame(temp$mapflag)[ , mergevars$similar2])
              if (FALSE %in% temp$check){
                temp$msg <- paste("The variable selected for the denominator",
                                  "cannot have values of zero.",
                                  "Please try again.")
                tcltk::tkmessageBox(title = "Please re-check variables",
                                    message = temp$msg,
                                    type = "ok", icon = "warning")
                temp$error <- TRUE # re-run step
              } else {
                step <- step + 1
              }
            }
          }
        }
      }
    } # merge type (mergevars)
    while (step ==  7) {
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      if (is.null(gatvars$popvar)) gatvars$popvar <- "NONE"
      if (is.null(filevars$popin)) filevars$popin <- filevars$userin

      if (mergevars$centroid == "population-weighted") {
        temp$error <- TRUE
        while (temp$error) {
          temp$msg <- "Select the population shapefile"
          tempfiles <- locateGATshapefile(msg = temp$msg,
                                          myfile = filevars$popin, step = step,
                                          bgcol = bgcol, buttoncol = buttoncol)
          temp$error <- FALSE
          if (tempfiles$userin == "cancel") {
            gatvars$popvar <- "back"
          } else {
            filevars$popin <- tempfiles$userin
            filevars$popfile <- tempfiles$filein
            filevars$poppath <- tempfiles$pathin
            temp$pop <- sf::st_read(dsn = filevars$poppath,
                                    layer = filevars$popfile)
            temp$polys <- sum(grepl("POLYGON", class(temp$pop$geometry),
                                    fixed = TRUE)) > 0
            temp$popnumvars <- checkGATvariabletypes(data.frame(temp$pop),
                                                     type = "number")

            # add dialog specifying each issue
            # add check for shapefile type (if class(shp) )
            if (!temp$polys) {
              # message: wrong kind of shapefile; repeat dialog
              temp$msg <- "The shapefile must contain polygons to be used."
              tcltk::tkmessageBox(title = "Shapefile invalid", type = "ok",
                                  icon = "error", message = temp$msg)
              temp$error <- TRUE
            } else if (!is.null(temp$popnumvars)) {
              gatvars$popvar <- identifyGATpopulation(varlist = temp$popnumvars,
                                                      step = step, bgcol = bgcol,
                                                      var = gatvars$popvar,
                                                      backopt = !temp$flagconfirm,
                                                      quitopt = quitopt,
                                                      buttoncol = buttoncol)
            }
          }
          if (gatvars$popvar == "back") {
            step <- step - 1
          } else {
            if (temp$flagconfirm) {
              step <- 11
            } else {
              temp$pop <- temp$pop[!sf::st_is_empty(temp$pop),]

              step <- step + 1
            }
          }
          rm(tempfiles)
        }
      } else {
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      }
    } # population weighting
    while (step ==  8) { # step 9: get rate settings
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      if (length(temp$numerics) > 1) {
        temp$error <- TRUE

        if (!exists("ratevars")) ratevars <- list()

        while (temp$error) {
          ratevars <- inputGATrate(shp = temp$mapflag, limitdenom = limitdenom,
                                   step = step, ratevars = ratevars,
                                   backopt = !temp$flagconfirm, bgcol = bgcol,
                                   quitopt = quitopt, buttoncol = buttoncol)
          temp$error <- FALSE

          # returns list(multiplier, ratename, numerator, denominator, colorscheme)
          if (!exists("ratename", ratevars)) {
            x <- confirmGATquit()
            if (x == "quit") {
              ratevars <- list(ratename = "cancel")
            } else {
              temp$error <- TRUE
              ratevars <- list(ratename = "gat_rate")
            }
          }
          if (ratevars$ratename == "cancel") {
            mysettings$quit <- TRUE
            step <- 20
          } else if (ratevars$ratename == "back") {
            step <- step - 1
          } else if (!temp$error) {
            if (ratevars$ratename == "no_rate") {
              #temp$msg <- "You have chosen not to calculate a rate."
            } else {
              if (nchar(ratevars$ratename) > 10) {
                # include message stating name will be shortened to 10 chars
                temp$msg <- paste("The rate name must be 10 characters or less.",
                                  "\n", ratevars$ratename, "has been shortened to",
                                  substr(ratevars$ratename, 1, 10))
                tcltk::tkmessageBox(title = "Rate name too long",
                                    message = temp$msg,
                                    type = "ok", icon = "warning")
                ratevars$ratename <- substr(ratevars$ratename, 1, 10)
              }
              # quality control - force numeric
              while (grepl("[^0-9.,-]", ratevars$multiplier)) {
                gats <- list(title = paste("Multiplier for", ratevars$ratename),
                             msg = paste0("Please enter a valid number for the ",
                                          "multiplier for ", ratevars$ratename, "."),
                             help = paste0("Enter a valid number. \n",
                                           "  \u2022  To continue,  click 'Next >'. \n",
                                           "  \u2022  To return to rate settings, click '< Back'.",
                                           "  \u2022  To quit GAT, click '", quitopt, "'."))
                ratevars$multiplier <- inputGATvalue(title = gats$title,
                                                     help = gats$help,
                                                     message = gats$msg,
                                                     defaulttext = "10,000",
                                                     helppage = "inputGATvalue",
                                                     step = step, bgcol = bgcol,
                                                     backopt = !temp$flagconfirm,
                                                     quitopt = quitopt,
                                                     buttoncol = buttoncol)
                if (ratevars$multiplier == "back") {
                  ratevars$ratename <- "back"
                  ratevars$multiplier <- 1
                } else if (ratevars$multiplier == "cancel") {
                  ratevars$ratename <- "cancel"
                  ratevars$multiplier <- 1
                  mysettings$quit <- TRUE
                  step <- 20
                }
              }
              ratevars$multiplier <- as.numeric(gsub(",", "", ratevars$multiplier))

              # quality control - force selection of different numerator and denominator
              # create a dialog specifically for this?
              if (!exists("numerator", ratevars) | !exists("denominator", ratevars) |
                  length(ratevars$numerator) == 0 | length(ratevars$denominator) == 0) {
                temp$msg <- "Please select different variables for numerator and denominator"
                tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                    type = "ok", icon = "warning")
                ratevars$ratename <- "gat_rate"
                temp$error <- TRUE
              } else if (ratevars$numerator == ratevars$denominator) {
                temp$msg <- "Please reselect the numerator and denominator"
                tcltk::tkmessageBox(title = "Please re-check variables", message = temp$msg,
                                    type = "ok", icon = "warning")
                ratevars$ratename <- "gat_rate"
                temp$error <- TRUE
              }
              #temp$msg <- paste0("You have chosen to calculate the rate ",
              #                   ratevars$ratename, " from ", ratevars$numerator,
              #                   " and ", ratevars$denominator, " with multiplier ",
              #                   format(as.numeric(ratevars$multiplier), big.mark=",",
              #                          scientific=FALSE),
              #                   " using the color scheme ", ratevars$colorname,
              #                   ". Is this correct?")

            }
          }
        }
        #if (!ratevars$ratename %in% c("back", "cancel")) {
        #  temp$title <- "Confirm your rate calculation settings"
        #  temp$x <- tcltk::tkmessageBox(title = temp$title, message = temp$msg,
        #                                type = "okcancel", icon = "question")
        #  temp$confirm <- tcltk::tclvalue(temp$x)

        #  if (temp$confirm == "ok") {
        #    if (temp$flagconfirm) {
        #      step <- 11
        #    } else {
        #      step <- step + 1
        #    }
        #  }
        #}
        if (ratevars$ratename == "back") {
          step <- step - 1
        } else if (ratevars$ratename == "cancel") {
          step <- 20
          mysettings$quit <- TRUE
        } else if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      } else {
        # message: wrong kind of shapefile; repeat dialog
        temp$msg <- "There are not enough numeric variables to calculate a rate."
        tcltk::tkmessageBox(title = "Rate calculation not possible", type = "ok",
                            icon = "warning", message = temp$msg)
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
        ratevars <- list(ratename = "no_rate")
      }
    } # rate (ratevars)
    while (step ==  9) {
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      temp$kml <- saveGATkml(step = step, backopt = !temp$flagconfirm, bgcol = bgcol,
                             quitopt = quitopt, buttoncol = buttoncol)

      if (temp$kml %in% c("Yes", "No")) {
        if (temp$kml == "Yes") {
          gatvars$savekml <- TRUE # save the kml
        } else {
          gatvars$savekml <- FALSE # save the kml
        }
        if (temp$flagconfirm) {
          step <- 11
        } else {
          step <- step + 1
        }
      } else if (temp$kml == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else {
        step <- step - 1
      }
    } # save KML
    while (step == 10) { # identify the save files' name and location
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      saves <- saveGATfiles()
      filevars$userout <- saves$userout
      filevars$fileout <- saves$fileout
      filevars$pathout <- saves$pathout

      if (filevars$fileout == "cancel") {
        mysettings$quit <- TRUE
        step <- 20
      } else {
        step <- step + 1
      }
    } # save file
    while (step == 11) { # add dialog to confirm merge settings
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      temp$flagconfirm <- TRUE
      temp$error <- TRUE
      gatvars$numrow <- nrow(temp$shp)
      while (temp$error) {
        temp$cancel <- confirmGATbystep(gatvars = gatvars, ratevars = ratevars,
                                        exclist = exclist, mergevars = mergevars,
                                        filevars = filevars, step = step,
                                        bgcol = bgcol, quitopt = quitopt,
                                        buttoncol = buttoncol)
        temp$error <- FALSE
        if (is.null(temp$cancel)) {
          x <- confirmGATquit()
          if (x == "quit") {
            temp$cancel <- "Yes"
            mysettings$quit <- TRUE
          } else {
            temp$error <- TRUE
          }
        }
      }
      if (temp$cancel %in% c("Yes", "None")) {
        step <- 20 # done with user input
        myshps$original <- temp$shp
        myshps$pop <- temp$pop
        # add population file
      } else if (temp$cancel == "back") { # now irrelevant
        step <- step - 1 # go back one
      } else if (temp$cancel == "cancel") {
        step <- 20
        mysettings$quit <- TRUE
      } else if (grepl("[0-9]", temp$cancel)) {
        step <- as.numeric(gsub("[^0-9]", "", temp$cancel))
      }
    } # settings confirmation
  } # end while (step < 20)
  #     end user input ----

  if (!mysettings$quit) { # here onward is automated
    #  3. prep shapefile ----
    step <- 14 # reset after the while loop
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    # housekeeping
    if (gatvars$aggregator2 == "NONE") gatvars$aggregator2 <- gatvars$aggregator1
    if (gatvars$myidvar == "missing") {
      myshps$original$temp_id <- paste0("ID_", 1:nrow(myshps$original))
      gatvars$myidvar <- "temp_id"
    }

    myshps$original <- myshps$original[!sf::st_is_empty(myshps$original),]
    sf::st_agr(myshps$original) <- "constant"
    temp$pts <- sf::st_centroid(myshps$original)

    sf::st_agr(temp$pts) <- "constant"
    sf::st_geometry(temp$pts) <- sf::st_centroid(temp$pts$geometry)
    temp$pts <- data.frame(do.call(rbind, sf::st_geometry(temp$pts)))
    colnames(temp$pts) <- c("GATx", "GATy")

    mapvars <- list(
      projection = sum(grepl("longlat", sf::st_crs(myshps$original), fixed = TRUE),
                       sf::st_crs(myshps$original, parameters = TRUE)$units_gdal %in%
                       c("Degree", "degree", "DEGREE")) > 0,  # returns boolean
      centroids = temp$pts[, c("GATx", "GATy")])

    # if projection is lat/long, projection = TRUE, otherwise FALSE
    # default to not lat/long if something goes wrong
    if (is.na(mapvars$projection)) mapvars$projection <- FALSE

    #  4. run aggregation loop ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    gatvars$popwt <- mergevars$centroid == "population-weighted"

    # defineGATmerge failed :(
    # check geographic weighting and similar ratio options
    # area = myshps$original; pop = myshps$pop
    aggvars <- defineGATmerge(area = myshps$original, gatvars = gatvars,
                              mergevars = mergevars, pop = myshps$pop,
                              pwrepeat = pwrepeat, adjacent = adjacent,
                              exclist = exclist, minfirst = minfirst)

    #  5. aggregate areas ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    myshps$aggregated <- mergeGATareas(ratevars = ratevars, aggvars = aggvars,
                                       idvar = "GATid", myshp = myshps$original)

    #  6. calculate compactness ratio ####
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    # to get maximum distance (diameter of circle): max(dist(test1))
    myshps$aggregated$GATcratio <- calculateGATcompactness(myshp = myshps$aggregated)

    #  7. map first variable: before and after ####
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    # create a list to record plots to be saved to pdf at the end.
    myplots <- list()

    
    # plot first aggregation variable
    myplots <- plotGATaggs(step = step, closemap = closemap, areaold = myshps$original, areanew = myshps$aggregated,
                           aggvar = gatvars$aggregator1, gatvars = gatvars, exclist = exclist, myplots = myplots,
                           mapvars = mapvars)
    

    #  8. map second variable: before and after ####
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    
      # plot second aggregation variable, if relevant
    myplots <- plotGATaggs(step = step,  closemap = closemap, areaold = myshps$original,
                             areanew = myshps$aggregated, aggvar = gatvars$aggregator2,
                           gatvars = gatvars, exclist = exclist, myplots = myplots,  mapvars = mapvars)
  

    #  9. map differences between old and new areas ####
    step <- step + 1
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

    # plot new and old on same map
    myplots$compare <-  plotGATcompare(areaold = myshps$original,
                                       areanew = myshps$aggregated,
                                       mergevars = mergevars,
                                       gatvars = gatvars,
                                       closemap = closemap)

    # 10. map compactness ratio ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)
    
    # create thematic map of compactness ratios
    gats <- list(title.main = "Compactness Ratio After Merging",
                 title.sub = paste("compactness ratio = area of polygon over",
                                   "area of circle with same perimeter \n",
                                   "1=most compact, 0=least compact"))
    myplots$compactness <- plotGATmaps(area = myshps$aggregated,
                                       var = "GATcratio", clr = "YlOrBr",
                                       title.main = gats$title.main,
                                       title.sub = gats$title.sub,
                                       ratemap = TRUE,
                                       closemap = closemap)

    # 11. map rates if needed ----
    step <- step + 1
    if (ratevars$ratename != "no_rate") { # map the rate, choropleth map
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

       myplots$rate <- plotGATrate(ratevars = ratevars, myshps = myshps, closemap = closemap, exclist = exclist)
         
         
    } # end mapping new rate

    # 12. save maps to pdf ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    # save the plots to a pdf file
    grDevices::pdf(paste0(filevars$userout, "plots.pdf"), onefile=TRUE,
                   width = 10, height = 7)
    for (myplot in myplots) {
      if (is(myplot, "recordedplot")) grDevices::replayPlot(myplot)
    } # only saves plots that exist
    grDevices::dev.off() # need to close pdf file

    rm(myplots)

    # save relevant objects
    save(file = paste0(filevars$userout, "settings.Rdata"),
         list = c("gatvars", "aggvars", "filevars", "mergevars", "ratevars",
                  "exclist", "mysettings"))

    # 13. save crosswalk shapefile ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    myshps$original$GATid <- aggvars$IDlist # add crosswalk IDs
    # warnings don't make sense; they say data not written successfully,
    # but shapefile is fine and data match original file
    sf::st_write(myshps$original, filevars$pathout, driver = "ESRI Shapefile",
                 paste0(filevars$fileout, "in"), delete_layer = TRUE)

    # 14. save new shapefile ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    names(myshps$aggregated) <- substr(names(myshps$aggregated), 1, 10)
    sf::st_write(myshps$aggregated, filevars$pathout, filevars$fileout,
                 driver = "ESRI Shapefile", delete_layer = TRUE)

    # 15. save kml file ----
    if (gatvars$savekml==TRUE) { # now includes descriptions
      step <- step + 1
      drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
                aggvars = aggvars, ratevars = ratevars)

      writeGATkml(myshp = myshps$aggregated, filename = filevars$fileout,
                  filepath = filevars$pathout, myidvar = gatvars$myidvar)
    }
    # 16. save log file ----
    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)

    mysettings$exists = file.exists(paste0(filevars$userout, ".shp"))
    writeGATlog(gatvars = gatvars, aggvars = aggvars, filevars = filevars,
                mysettings = mysettings, area = myshps$original,
                mergevars = mergevars, ratevars = ratevars, exclist = exclist)

    step <- step + 1
    drawGATpb(gatenv = gatenv, step = step, mysettings = mysettings, filevars = filevars,
              aggvars = aggvars, ratevars = ratevars)


    if (mysettings$exists) {
      msg <- paste0("NYS GAT is finished. Your files were saved to ",
                    filevars$pathout,
                    ". \nPlease see the log file for more details.")
      tcltk::tkmessageBox(title = "GAT finished", type = "ok",
                          icon = "info", message = msg)

      msg <- paste0("\n\nThe following files have been written to the folder \n",
                    filevars$pathout, ": \n  ",
                    filevars$fileout, ".dbf \n  ",
                    filevars$fileout, ".prj \n  ",
                    filevars$fileout, ".shp \n  ",
                    filevars$fileout, ".shx \n  ",
                    filevars$fileout, "in.dbf \n  ",
                    filevars$fileout, "in.prj \n  ",
                    filevars$fileout, "in.shp \n  ",
                    filevars$fileout, "in.shx \n  ",
                    filevars$fileout, "plots.pdf \n  ",
                    filevars$fileout, ".log \n  ",
                    filevars$fileout, "settings.Rdata \n  ")
      if (gatvars$savekml==TRUE) {
        msg <- paste0(msg,
                      filevars$fileout, ".kml \n",
                      filevars$fileout, ".kmz \n")
      }
      msg <- paste0(msg, "\nSee the log file for more details.")

      message(msg)
    } else {
      # the shapefile failed to write
      msg <- "Something went wrong. Your shapefiles were not saved."
      tcltk::tkmessageBox(title = "Shapefile save failed", type = "ok",
                          icon = "error", message = msg)
    }
    
    
  } else {
    # "else" occurs only if GAT is cancelled in the input phase
    # there are now redundant warnings; function-level are commented out
    msg <- "You have chosen to cancel GAT."
    tcltk::tkmessageBox(title = "GAT cancelled",
                      message = msg,
                      type = "ok", icon = "warning")
    
  }
  # end GAT ####
  close(gatenv$tpb)
}

