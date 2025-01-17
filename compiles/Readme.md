# Geographic Aggregation Tool (GAT) Change Log


## [GAT v2.0.0](gatpkg_2.0.0.tar.gz) (March 9, 2023)

> Use this version of GAT for R version 4.0 and above.
> You must use this version of GAT for R version 4.2 and above.

* Added advanced options to the settings file and log.
* Converted all instances of rgeos, rgdal, lwgeom, foreign, and sp to sf and removed those dependencies. 
* Converted all uses of tcltk2 to tcltk and removed dependency on tcltk2 as it was crashing RStudio with R4.2.0.
* Revised and standardized all pop-up fonts and colors except the pre-defined progress bar and file-selection window.
* Revised writeGATkml() to use sf and removed dependency on plotKML.
* Revised population weighting to read in the population file only once.
* Removed checkGATshapefilesize() as support for memory.limit() is ending. 
* Added a 'GATid' variable to clone the selected merge variable instead of overwriting it.
* Fixed the bug that incorrectly assigned GATid for the crosswalk and messed up the final aggregated areas.
* Fixed the bug that caused GAT to crash if the user prematurely closed the progress bar.
* Clarified possible issues in the troubleshooting vignette.
* Fixed bugs in defineGATmerge() and identifyGATfirstobs() that caused GAT to crash.

## [GAT v1.62.0](gatpkg_1.62.0.tar.gz) (January 1, 2023)

> Use this version of GAT for R versions 3.5 to 4.1. 

* Minor edits. Last GAT version before the overhaul.

## GAT v1.61.2 (April 13, 2022)

* Patched to replace plotKML in writeGATkml with sf::st_write because plotKML was removed from CRAN.

## GAT v1.61.1 (January 4, 2022)

* Added structure for pkgdown.
* Fixed typos and formatting issues in documentation.


## [GAT v1.61.0](gatpkg_1.61.0.tar.gz) (February 8, 2021)

* Added vignette "Assessing GAT Results".
* Fixed bugs and typos in confirmGATbystep() and runGATprogram(), notably the 
  bug that incorrectly displayed number of excluded areas over maximum.
* Added objects "ismax1", "ismin2", and "ismax2" to the gatvars list object
  to track if minimum and maximum values were defined by the user and added
  this information to the log.
* Created a separate section in the technical notes to describe variables
  created by GAT.
* Moved citation and acknowledgements from set-up to a separate vignette.
* Restructured locateGATshapefile() to add a modifiable status bar option.
* Added and revised several function examples.
* Added the following function options (and corresponding code in runGATprogram):
    * writeGATlog: added settingsfile option to allow incorrectly written logs 
      to be regenerated from the settings.Rdata file produced by GAT
    * defineGATmerge: added progressbar option to suppress the progress bar if
      desired
    * plotGATmaps and plotGATcompare: added closemap option to automatically 
      close map windows if desired
* Dropped compatibility for R-3.4.0.

## GAT v1.60.3 (August 31, 2020)

* Added vignette "Preparing your shapefile for GAT".

## GAT v1.60.2 (August 14, 2020)

* Completed bug fixes in defineGATmerge.
* Added function aggregateGATnb, which is used in defineGATmerge.

## GAT v1.60.1 (August 6, 2020)

* Addressed bug fixes in defineGATmerge.
* Added XML and network saving issues to the troubleshooting document.

## [GAT v1.60.0](gatpkg_1.60.0.tar.gz) (Jan 2019 - Jul 2020)

> Use this version of GAT for R version 3.4 to 3.6.

All changes from January 2019 onward by Abigail Stamm.

> Below is a summary of differences between the R script for GAT released in 
> 2015 and the first beta release of the R package for GAT in 2020.

#### Coding changes

* Reordered GAT steps by section: user input, aggregation, mapping, and 
  file saving.
* Converted all instances of svdialogs to tcltk and where possible, 
  upgraded tcltk code to tcltk2.
* Rewrote KML code to use plotKML(), which sped it up considerably.
* Rewrote and expanded quit code to request quit confirmation and not 
  crash GAT.
* Standardized GAT-created variable names.
* Made GAT ID length dynamic and consistent.
* Expanded error messaging and improved its precision.
* Added dialogs to handle incorrectly entered data on the fly.


#### Resource changes

* Created over 30 functions with accompanying help files and examples.
* Developed several vignettes, including a tutorial, and expanded the 
  technical notes and troubleshooting documents.
* Replaced statewide and Albany shapefiles with two smaller shapefiles, 
  hftown and hfblock, and updated examples to use only these files.
* Removed simulated birth data and references to it.
* Added citation, license, and funding information.
* Added a function to combine results after running a shapefile through 
  GAT, then running its result through GAT.


#### User input changes

* Reordered and restructured user input dialogs.
* Added help button and instructions to user input dialogs.
* Added function options to user input dialogs to allow the following:
    * maximum values for up to 2 aggregation variables
    * optional enforced boundaries for 1 boundary variable
    * optional enforced adjacent areas
    * population weighting using 2 methods
    * up to 3 exclusion criteria (numeric only)
    * option not to write KML file
    * saving and rerunning prior settings
    * confirmation dialog that allows user to return to previous steps


#### Output changes

* Added aggregation and summary information to maps.
* Added map layers for excluded and problematic areas.
* Saved all maps to a PDF file.
* Added all settings, aggregation warnings, and GAT-created variables and 
  files to the log. 
* Added a list of saved files to the console.
* Saved GAT settings in an Rdata object to be reread into GAT.
* Made saving to KML optional.




## GAT v1.34 (Release date: Aug 28, 2015)

All changes prior to 2019 by Gwen Babcock. For the latest versions of Gwen's 
files, visit the [archive](../archive).

> Changes prior to 2019 are listed below as they were entered in the program
> comments, with minor formatting changes. Most do not appear to correspond
> directly to specific versions. Comments appear to cover a mix of actual and
> planned changes. 


* Jan 11, 2009 modified to add kml output
    * want to add capability to use two variables, and to merge from the
      highest down instead of lowest up
    * modify to use rgdal to read & write shapefile. It allows for projection
      info, but does have other limitations
    * Note: need to explicitly specify any libraries needed to run as a batch
      file, even those that would ordinarily be loaded by default
    * have problem with svDialog list.  We hypothesize that it can bomb if
      the list contains less than 3 items so modify the program accordingly
* April 20, 2009 modify to put compactness ratio measure in output data
* beta v5 June 21, 2010 change dialogs to add 'back' and 'help' options
* beta v6 July 15, 2010 add option to calculate rate, added popup when
  aggregation occurs to warn of slowness
    * added column with number of areas per region
    * add comparison of file size with memory availability
* beta v6.1 December 7, 2010 add conditional code to run gpclibPermit()
  function if needed
* December 10, 2010 change name to production version 1.0
* Dec 14, 2010 add merging option to most similar neighbor
* May 2011 v 1.2 bug fixes: check for errors in rate calculations and remove
  extra linking variable before re-exporting input shapfile
* October 27, 2011 found bug: will not export shapefile if field names are
  more than 10 characters
    * fix this by adding substring function to reduce variable name length if
      needed
* Sept 27,2012 modify to not standardize compactness ratios
* Feb 13, 2013 include version number and date in message, so it will be
  included in the log
* Aug 15, 2013 modify to work in R 3.0.1
    * That looks like it will take a lot of work
    * some packages I used before are no longer available, such as gpclib
    * svDialogs has changed - new function names
    * add progress dialog
    * also make KML output always, reprojecting if needed
    * need to wait for listboxes to be visible before populating them
* October 31, 2013 add code to make similarity based on a ratio, not a count
* November 5, 2013 change logic to merge in same specified boundary whenever
  possible, even if not contiguous
* November 7, 2013 test rates and similarity ratios for zero denominators.
  Don't allow them for similarity, but plot the rates while ignoring bad
  values
* November 8, 2013 change rate plot to show integers only, and use jenks
  (natural breaks) because quintiles was not working
* Feb 5, 2014 fix bugs in transformations - wrong formula to calculate UTM
  zones; rename version to v1.31
* June 13, 2014 add filter to open dialog to limit view to shapefiles.
* April 10, 2015 fix problem: limit dialog messages to 255 characters
* August 28, 2015 fix typo mergoption->mergeoption
* November 20, 2015 fix typo indentified->identified

## Package notes as of GAT v1.34

1. attached base packages:
    * tcltk
    * stats
    * graphics
    * grDevices
    * utils
    * datasets
    * methods
    * base
1. other attached packages:
    * Matrix_1.0-12
    * lattice_0.20-15
    * rgdal_0.8-10
    * foreign_0.8-53
    * svDialogs_0.9-54
    * svGUI_0.9-54
    * classInt_0.1-20
    * e1071_1.6-1
    * class_7.3-8
    * RColorBrewer_1.0-5
    * maptools_0.8-26
    * rgeos_0.2-19
    * sp_1.0-11
1. loaded via a namespace (and not attached):
    * boot_1.3-9
    * coda_0.16-1
    * deldir_0.0-22
    * grid_3.0.1
    * LearnBayes_2.12
    * MASS_7.3-26
    * nlme_3.1-109
    * spdep_0.5-62
    * splines_3.0.1
