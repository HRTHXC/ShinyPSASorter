

shinyServer(function(input, output, session) {
  
  controlVar <- reactiveValues(fileUploaded = FALSE, outputTable = TRUE)
  dat <- NULL
  
  observeEvent(input$upload_file, {
    controlVar$fileUploaded <- FALSE
    if (is.null(input$upload_file))
      return()
    inFile <- input$upload_file
    dat <<- fread(inFile$datapath)
    if(!is.data.frame(dat))
      return(dat)
    controlVar$fileUploaded <- TRUE
  })
  
  observeEvent(input$tableFilter, {
    print(dat$BrCrCode)
    print(input$tableFilter)
    dat <- subset(dat, !(BrCrCode %in% input$tableFilter))
    print(dat$BrCrCode)
    cleanFile(dat)
  })
  
  omitRows <- reactive({
    switch(input$tableFilter,
           "omission" = omission)
  })
  
  # ntext?
  ntext <- eventReactive(input$goButton, {
    omitRows(input$tableFilter)
    print(input$tableFilter)
  })
  
  cleanFile <- function(dat){
    testParents <- mutate(select(dat, BrCrCode, Planted, nonPsa, PsaDeaths), MotherCode = '', FatherCode = '')
    slashCount <- str_count(testParents$BrCrCode, '/')
    underscoreCount <- str_count(testParents$BrCrCode, '_')
    spaceCheck <- str_count(testParents$BrCrCode, ' ')
    testParents <- cleanBrCrCodeInFile(testParents, slashCount, underscoreCount, spaceCheck)
    testParents$Planted <- as.numeric(testParents$Planted)
    testParents$PsaDeaths <- as.numeric(testParents$PsaDeaths)
    testParents$nonPsa <- as.numeric(testParents$nonPsa)
    testParents$survivalrate <- format(round((value = 100 - ((testParents$PsaDeaths / (testParents$Planted - testParents$nonPsa) ) * 100)), 2), nsmall = 2)
    return(testParents)
  }
  
  overallSurvival <- function(testParents){
    overallSurvival <- format(round( 100 - ((sum(as.numeric(testParents$PsaDeaths)) / sum(as.numeric(testParents$Planted - testParents$nonPsa))) * 100), 2), nsmall = 2)
    return(overallSurvival)
  }
  
  # gives no improvement in execution time?
  # genericBrCrCodeCase <- function(x, i){
  #   x$BrCrCode[i] <- paste(x$MotherCode[i], x$FatherCode[i], sep = "")
  #   return(x)
  # }
  
  cleanBrCrCodeInFile <- function(x, slashCount, underscoreCount, spaceCheck){
    for(i in 1:(dim(x)[1])){
      if(spaceCheck[i]==1){
        x$BrCrCode[i] <- gsub(" ", "", x$BrCrCode[i])
        underscoreCount[i] = 2
      }
      if(slashCount[i]==1) {
        if(underscoreCount[i]==1){
          if(nchar(unlist(strsplit(x$BrCrCode[i], split = '/', fixed=TRUE))[2]) == 3){
            next 
          }
          else{
            x$MotherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '/', fixed=TRUE))[1]
            x$BrCrCode[i] <- x$MotherCode[i]
          }
        }
        x$MotherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '_', fixed=TRUE))[1]
        x$FatherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '_', fixed=TRUE))[2]
        idk <- nchar(unlist(strsplit(x$FatherCode[i], split = '/', fixed=TRUE))[2])
        idk[is.na(idk)] <- 0
        if(idk == 1){
          x$FatherCode[i] <- unlist(strsplit(x$FatherCode[i], split = '/', fixed=TRUE))[1]
        }
      }
      if(x$BrCrCode[i]=="") {
        x$MotherCode[i] <- "PH"
        x$FatherCode[i] <- paste(i, "ph", sep = "")
        x$BrCrCode[i] <- paste(x$MotherCode[i], x$FatherCode[i], sep = "")
      }
      if(slashCount[i]==3) {
        x$MotherCode[i] <- "control"
        x$FatherCode[i] <- unlist(strsplit(x$BrCrCode[i], split = '/', fixed=TRUE))[1]
        x$BrCrCode[i] <- paste(x$MotherCode[i], x$FatherCode[i], sep = "") 
      }
      if(underscoreCount[i] == 0) {
        x$MotherCode[i] <- as.character(str_extract(x$BrCrCode[i], "[aA-zZ]+"))
        x$FatherCode[i] <- as.character(str_extract(x$BrCrCode[i], "[0-9]+"))
        x$BrCrCode[i] <- paste(x$MotherCode[i], "_", x$FatherCode[i], sep = "")
      }
    }
    return(x)
  }
  
  verbatimSanity <- function(y){
    y$MotherCode <- y$FatherCode <- y$survivalrate <- NULL
    y <- mutate(aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode, data = y, FUN = sum), survivalrate = round((100 - (PsaDeaths / (Planted - nonPsa)) * 100), 2))
    updateSelectizeInput(session, 'tableFilter', choices = y$BrCrCode) 
    return(y)
  }
  
  # to fix, something is potentially wrong here
  doubleUnderscoreCheck <- function(z, testMothers, testFathers, underscoreCount){
    thirdIndexDetection <- which(sapply(strsplit(paste(z$BrCrCode, 1:length(z$BrCrCode), sep="_"), "_"), function(x)x[[2]]) == "")
    z$MotherCode <- testMothers$BrCrCode <- sapply(strsplit(z$BrCrCode, split = '_', fixed=TRUE), function(x)x[[1]])
    z$FatherCode <- testFathers$BrCrCode <- sapply(strsplit(paste(z$BrCrCode, 1:length(z$BrCrCode), sep="_"), "_"), function(x)x[[2]])
    # for(i in 1:length(thirdIndexDetection)){
    #   z$FatherCode[thirdIndexDetection] <- testFathers$BrCrCode[thirdIndexDetection] <- sapply(strsplit(paste(z$BrCrCode[thirdIndexDetection[i]], 1:length(z$BrCrCode), sep="_"), "_"), function(x)x[[2]])
    # }
    return(z)
  }
  
  #turn these two into one function, you dumbass...
  mothersTable <- function(testMothers, testParents){
    testMothers$BrCrCode <- testParents$MotherCode
    testMothers <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode, data = testMothers, FUN = sum)
    testMothers <- mutate(testMothers, survivalrate = round((100 - (PsaDeaths / (Planted - nonPsa)) * 100), 2))
    return(testMothers)
  }

  fathersTable <- function(testFathers, testParents){
    testFathers$BrCrCode <- testParents$FatherCode
    testFathers <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode, data = testFathers, FUN = sum)
    testFathers <- mutate(testFathers, survivalrate = round((100 - (PsaDeaths / (Planted - nonPsa)) * 100), 2))
    return(testFathers)
  }
  
  # creationTable <- function(parentTable, parentCode){
  #   parentTable$BrCrCode <- testParents$parentCode
  #   parentTable <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode, data = parentTable, FUN = sum)
  #   parentTable <- mutate(parentTable, survivalrate = round((100 - (PsaDeaths / (Planted - nonPsa)) * 100), 2))
  #   return(parentTable)
  # }
  
  twoDTableCreation <- function(testMothers, testFathers, testParents, testParentsVerbaitum){
    twoDtable <- matrix(c(""), nrow=length(testMothers$BrCrCode), ncol=length(testFathers$BrCrCode))
    rownames(twoDtable) <- testMothers$BrCrCode
    colnames(twoDtable) <- testFathers$BrCrCode
    l <- 1
    m <- 0
    for(i in 1:length(testParents$BrCrCode)){
      motherCodeFound <- 0
      fatherCodeFound <- 0
      motherCodeFoundString <- ""
      fatherCodeFoundString <- ""
      for(j in 1:length(testMothers$BrCrCode)){
        if(grepl(testMothers$BrCrCode[j], testParents$MotherCode[l])){
          motherCodeFound <- j
          motherCodeFoundString <- testMothers$BrCrCode[j]
          break
        }
      }
      for(k in 1:length(testFathers$BrCrCode)){
        if(grepl(testFathers$BrCrCode[k], testParents$FatherCode[l])){
          fatherCodeFound <- k
          fatherCodeFoundString <- testFathers$BrCrCode[k]
          m <- m + 1
          break
        }
      }
      for(n in 1:length(testParentsVerbaitum$BrCrCode)){
        if(grepl(testParentsVerbaitum$BrCrCode[n], testParents$BrCrCode[i])){
          ifelse(testParentsVerbaitum$Planted[n] < 40, (twoDtable[motherCodeFound, fatherCodeFound] = paste(testParentsVerbaitum$survivalrate[n], "STS", sep = " ")), (twoDtable[motherCodeFound, fatherCodeFound] = testParentsVerbaitum$survivalrate[n]))
          break;
        }
      }
      l <- l + 1
    }
    twoDtableDF <- data.frame(twoDtable)
    return(twoDtableDF)
  }
  
  output$contents = renderTable({
    if (controlVar$fileUploaded || controlVar$outputTable){
      beginning <- Sys.time()
      testParents <- cleanFile(dat)
      overallSurvival <- overallSurvival(testParents)
      testMothers <- testFathers <- testParentsVerbaitum <- testParents <- aggregate(cbind(Planted, nonPsa, PsaDeaths) ~ BrCrCode+MotherCode+FatherCode+survivalrate, data = testParents, FUN = sum)
      testParents <- doubleUnderscoreCheck(testParents, testMothers, testFathers, underscoreCount)
      #something here is keeping unformatted brcrcodes in the table, which messes up the rest of the table
      testParentsVerbaitum <- verbatimSanity(testParentsVerbaitum)
      
      
      # output$x9 <- downloadHandler(
      #   filename = 'pdfs.zip',
      #   content = function(fname) {
      #     
      #     fs <- c("rock.csv", "pressure.csv", "cars.csv")
      #     write.csv(testParentsVerbaitum, file = "parents.csv", sep =",")
      #     write.csv(testMothers, file = "mothers.csv", sep =",")
      #     write.csv(testFathers, file = "fathers.csv", sep =",")
      #     write.csv(twoDtableDF, file = "2dmatrix.csv", sep =",")
      #     print (fs)
      #     
      #     zip(zipfile=fname, files=fs)
      #   },
      #   contentType = "application/zip"
      # )
      
      
      output$overallStats <- renderText(paste("In this block, ", sum(testParents$Planted), " plants were planted, and ", sum(testParents$PsaDeaths), " plants surcumb to PSA disease. ", sum(testParents$nonPsa), " individuals plants died of non PSA related issues. This is a survival rate of ~", overallSurvival, "%", sep = ""))
      output$x1 = DT::renderDataTable(testParentsVerbaitum, server = FALSE, filter = "bottom")
      output$x5 = downloadHandler('parents-filtered.csv', content = function(file) {
        s = input$x1_rows_all
        write.csv(testParentsVerbaitum[s, , drop = FALSE], file)
      })
      testMothers <- mothersTable(testMothers, testParents)
      output$x2 = DT::renderDataTable(testMothers, server = FALSE, filter = "bottom")
      output$x6 = downloadHandler('mothers-filtered.csv', content = function(file) {
        s = input$x2_rows_all
        write.csv(testMothers[s, , drop = FALSE], file)
      })
      testFathers <- fathersTable(testFathers, testParents)
      output$x3 = DT::renderDataTable(testFathers, server = FALSE, filter = "bottom")
      output$x7 = downloadHandler('fathers-filtered.csv', content = function(file) {
        s = input$x3_rows_all
        write.csv(testFathers[s, , drop = FALSE], file)
      })
      twoDtableDF <- twoDTableCreation(testMothers, testFathers, testParents, testParentsVerbaitum)
      output$x4 = DT::renderDataTable(twoDtableDF, server = FALSE)
      output$x8 = downloadHandler('overall2dmatrix.csv', content = function(file) {
        s = input$x4_rows_all
        write.csv(twoDtableDF[s, , drop = FALSE], file)
      })
      #grab sum of planted and psadeaths somehow idfkend <- Sys.time()
      print(end - beginning)
    }
  })
})