---
title: "ShinyPSASorter"
author: "Harrison Crane"
date: "5 December 2016"
output: html_document
---

## ShinyPSASorter

This is an R app written for the ShinySorter application, based from the VBScript written by Harrison Crane. It helps gives easier viewing of data of PSA death in the plantations here at PFR, allowing for those users to find the successful crosses of plants, and to cull those less successful crosses.

This is a final release (Shiny FINAL and VBScript are the best working versions, there were many different iterations). Input is via .csv document (.csv or .xls(x) for Excel), which can be downloaded from <a href="http://shiny.powerplant.pfr.co.nz/PsaBlockSummary/">here, under "Family consolidation"</a>, for use on your client machine.

email: Harrison.crane@plantandfood.co.nz

## VBScript release (.csv (preferred), .xlsx)

To install the VBScript of ShinyPSASorter, please follow the tutorial pictures in this link.

Text version (first time run):

1. Open your .csv or .xlsx file you intend to run the macro on.
2. On the main screen of excel, use keyboard command ALT+F11 to open the Visual Basic Editor.
3. Drag file calcAvg.bas into the "Project" tab of Visual Basic Editor. A new folder tab called "Modules" should appear.
4. Expand the "Modules" tab, and double click the enclosed module. A new internal window will open, with the source code. Now click the green start button, or press F5.
5. A dialogue window named "Macros" will appear. Confirm macro name is "calcAvg", and now press run in this dialogue window.
6. Program should take upwind of a few seconds to execute, and will end with a completion message. You may now use the tables and data to your liking.

Text version (after the first run):

1. Open your .csv or .xlsx file you intend to run the macro on.
2. On the main screen of excel, use keyboard command ALT+F11 to open the Visual Basic Editor.
3. Click the green start button in the Visual Basic Editor, or press F5.
4. Program should take upwind of a few seconds to execute, and will end with a completion message. You may now use the tables and data to your liking.

## Interim and FRead .Rmd releases (.csv only)

To install and use these versions of the script, you must have RStudio and an R library installed on your machine. Almost every machine at PFR should have such software.

In RStudio, you should also have certain packages install in order for the script to work. You can get those from the console in the RStudio application. Copy this into the console to get those repos.

```install.packages(c("knitr", "data.table", "plyr", "stringr"))```

Hit enter and let those repos download and install. It may take a little time, but then you can use the script. Total download required is about 3.8MB.

It should also be notice that due to human error, 2D MATRIX TABLES DO NOT EXIST IN THIS VERSION. SORRY. I did a dumb.

## Development release to Shiny (.csv only)

Relevant version being created can be found in "FINAL".

Upon loading the program, and clicking the file upload button, the user is asked to load in a correctly-formatted CSV file, which can be found at http://shiny-dev.powerplant.pfr.co.nz/PsaBlockSummary/ . It will then do all the work required and then give you the option to download to .csv file for all different subsets

Click the "Run" button on any of the .R files enclosed in the program, and then you can use the program.

For viewing the 2D tables, it's reccomended to download the table and open in excel, then using this useful trick:
<a href="http://stackoverflow.com/questions/10915733/freezing-row-1-and-column-a-at-the-same-time">Locking row and colnames at the same time</a>

Known bugs:

Nil (self-confidence for days...)