#' Input GAT Message
#'
#' @description
#' This function opens a dialog window for the user to confirm the contents
#' of the provided message.
#'
#' \figure{inputGATmessage.png}
#'
#' *Figure: Message dialog*
#'
#' Select your desired variables and the exclusion criterion for each.
#'
#' * Click \code{Yes} to confirm the message.
#' * Click \code{No} to refuse the message.
#' * Click \code{Back} to return to the previous step.
#' * Click \code{Help} to get further guidance and open the manual.
#'
#'
#' @param title     A text string that denotes the dialog title.
#' @param help      A text string containing the help message.
#' @param helppage  A text string that cntains the function name for the
#'                  relevant function (if any) in the help dialog.
#' @param step      Integer step in the GAT program, for help reference.
#' @param buttonopt A string that denotes the display value for the
#'                  no/cancel button.
#' @param msg       A text string that denotes the message for the user.
#' @param helptitle A text string denoting the title bar for the help window.
#' @param backopt   Boolean denoting whether to include the back button.
#'
#' @examples
#'
#' if (interactive()) {
#' # define your message
#' inputGATmessage(title = "Random message window")
#' }
#'
#' @export

inputGATmessage <- function(title = "GAT input window", msg = "Is GAT fun?",
                            help = "There is no help.",
                            helptitle = "inputGATmessage",
                            helppage = "inputGATmessage", step = 0,
                            buttonopt = "Cancel GAT", backopt = TRUE) {
  bgcol <- "lightskyblue3"
  buttoncol <- "cornflowerblue"

  tt <- tcltk::tktoplevel(background = bgcol)
  tcltk::tktitle(tt) <- paste0("Step ", step, ": ", title)
  tt$env$tm <- tcltk::tklabel(tt, text = msg, justify = "left",
                              background = bgcol)
  tcltk::tkgrid(tt$env$tm, sticky = "w", padx = 5, pady = 5)

  myenv <- new.env()

  onOk <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "Yes", envir=myenv)
  }
  onCancel <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "cancel", envir=myenv)
  }
  onHelp <- function() {
    showGAThelp(help = help, helptitle = helppage,
                helppage = helppage, step = step)
  }
  onBack <- function() {
    tcltk::tkdestroy(tt)
    assign("myvalue", "back", envir=myenv)
  }
  tt$env$tf <- tcltk::tkframe(tt, background = bgcol)

  if (backopt) {
    tt$env$tf$BackBut <- tcltk::tkbutton(tt$env$tf, text = "< Back",
                                         command = onBack, width = 12,
                                         background = buttoncol)
    tt$env$tf$OkBut <- tcltk::tkbutton(tt$env$tf, text = "Next >", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  } else {
    tt$env$tf$OkBut <- tcltk::tkbutton(tt$env$tf, text = "Confirm", width = 12,
                                       command = onOk, default = "active",
                                       background = buttoncol)
  }

  tt$env$tf$HelpBut <- tcltk::tkbutton(tt$env$tf, text="Help", width = 12,
                                       command = onHelp,
                                       background = buttoncol)
  tt$env$tf$CancelBut <- tcltk::tkbutton(tt$env$tf, text = buttonopt,
                                         width = 12, command = onCancel,
                                         background = buttoncol)

  tcltk::tkgrid(tt$env$tf, pady = 5)
  if (backopt) {
    tcltk::tkgrid(tt$env$tf$BackBut, column = 1, row = 1, pady = 5, padx = c(5, 0))
  }
  tcltk::tkgrid(tt$env$tf$OkBut, column = 2, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$CancelBut, column = 3, row = 1, pady = 5)
  tcltk::tkgrid(tt$env$tf$HelpBut, column = 4, row = 1, pady = 5, padx = c(0, 5))

  tcltk::tkwait.window(tt)

  return(myenv$myvalue)
}
