---
title: "ShinyPSASorterInterim"
author: "Harrison Crane"
date: "5 December 2016"
output: html_document
---

## ShinyPSASorterInterim

This is an R Markdown document written for the ShinySorter application, based from the VBScript written by Harrison Crane. It helps gives easier viewing of data of PSA death in the plantations here at PFR, allowing for those users to find the successful crosses of plants, and to cull those less successful crosses.

This is an interim release prior to the development of the Online Shiny release. Input is via .csv document, which can be downloaded from <a href="http://shiny.powerplant.pfr.co.nz/PsaBlockSummary/">here</a>, for use on your client machine. You can also use the VBScript release, instructions will come soon.

email: Harrison.crane@plantandfood.co.nz

## VBScript release (use Excel format, .xlsx)

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