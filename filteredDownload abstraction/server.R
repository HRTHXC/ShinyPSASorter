

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
  
  cleanFile <- function(dat){
    testParents <- mutate(select(dat, BrCrCode, Planted, PsaDeaths), MotherCode = '', FatherCode = '')
    slashCount <- str_count(testParents$BrCrCode, '/')
    underscoreCount <- str_count(testParents$BrCrCode, '_')
    spaceCheck <- str_count(testParents$BrCrCode, ' ')
    overallSurvival <- -(1 - ((sum(as.numeric(testParents$PsaDeaths)) / sum(as.numeric(testParents$Planted))) * 100))
    testParents <- cleanBrCrCodeInFile(testParents, slashCount, underscoreCount, spaceCheck)
    testParents$Planted <- as.numeric(testParents$Planted)
    testParents$PsaDeaths <- as.numeric(testParents$PsaDeaths)
    #find more suitable location to calculate a verbatium table?
    testParents$survivalrate <- format(round((value = 100 - ((testParents$PsaDeaths / testParents$Planted) * 100)), 2), nsmall = 2)
    return(testParents)
  }
  
  cleanBrCrCodeInFile <- function(x, slashCount, underscoreCount, spaceCheck){
    x$BrCrCode <- gsub(" ", "_", x$BrCrCode)
    if(x$BrCrCode=="") {
      x$MotherCode <- "PH_"
      x$FatherCode <- paste("1", "ph", sep = "")
      x$BrCrCode <- paste(x$MotherCode, x$FatherCode, sep = "")
    }
    if(slashCount==1) {
      x$BrCrCode <- x$MotherCode <- unlist(strsplit(x$BrCrCode, split = '/', fixed=TRUE))[1]
      x$FatherCode <- unlist(strsplit(x$BrCrCode, split = '/', fixed=TRUE))[2]
    }
    if(slashCount==3) {
      x$BrCrCode <- paste("control_", unlist(strsplit(x$BrCrCode, split = '/', fixed=TRUE))[1], sep = "")
      x$MotherCode <- "control"
      x$FatherCode <- unlist(strsplit(x$BrCrCode, split = '/', fixed=TRUE))[1]
    }
    if(underscoreCount == 0) {
      x$BrCrCode <- paste(as.character(str_extract(x$BrCrCode, "[aA-zZ]+")), "_", as.character(str_extract(x$BrCrCode, "[0-9]+")), sep = "")
      x$MotherCode <- as.character(str_extract(x$BrCrCode, "[aA-zZ]+"))
      x$FatherCode <- as.character(str_extract(x$BrCrCode, "[0-9]+"))
    }
    return(x)
  }
  
  verbatimSanity <- function(y){
    y$MotherCode <- y$FatherCode <- y$survivalrate <- NULL
    y <- mutate(aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = y, FUN = sum), survivalrate = round((100 - (PsaDeaths / Planted) * 100), 2))
    return(y)
  }
  
  doubleUnderscoreCheck <- function(z, testMothers, testFathers){
    underscoreCount <- str_count(z$BrCrCode, '_')
    if(underscoreCount==2){
      z$MotherCode <- testMothers$BrCrCode <- unlist(strsplit(z$BrCrCode, split = '_', fixed=TRUE))[1]
      z$FatherCode <- testFathers$BrCrCode <- unlist(strsplit(z$BrCrCode, split = '_', fixed=TRUE))[3]
    }
    z$MotherCode <- testMothers$BrCrCode <- unlist(strsplit(z$BrCrCode, split = '_', fixed=TRUE))[1]
    z$FatherCode <- testFathers$BrCrCode <- unlist(strsplit(z$BrCrCode, split = '_', fixed=TRUE))[2]
    return(z)
  }
  
  mothersTable <- function(testMothers, testParents){
    testMothers$BrCrCode <- testParents$MotherCode
    testMothers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testMothers, FUN = sum)
    testMothers <- mutate(testMothers, survivalrate = round((100 - (PsaDeaths / Planted) * 100), 2))
    return(testMothers)
  }
  
  fathersTable <- function(testFathers, testParents){
    testFathers$BrCrCode <- testParents$FatherCode
    testFathers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testFathers, FUN = sum)
    testFathers <- mutate(testFathers, survivalrate = round((100 - (PsaDeaths / Planted) * 100), 2))
    return(testFathers)
  }
  
  output$contents = renderTable({
    if (controlVar$fileUploaded || controlVar$outputTable){
      testParents <- cleanFile(dat)
      testMothers <- testFathers <- testParentsVerbaitum <- testParents <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode+MotherCode+FatherCode+survivalrate, data = testParents, FUN = sum)
      testParentsVerbaitum <- verbatimSanity(testParentsVerbaitum)
      testParents <- doubleUnderscoreCheck(testParents, testMothers, testFathers)
      output$x1 = DT::renderDataTable(testParentsVerbaitum, server = FALSE)
      # download the filtered data
      output$x5 = downloadHandler('parents-filtered.csv', content = function(file) {
        s = input$x1_rows_all
        write.csv(testParentsVerbaitum[s, , drop = FALSE], file)
      })
      testMothers <- mothersTable(testMothers, testParents)
      output$x2 = DT::renderDataTable(testMothers, server = FALSE)
      output$x6 = downloadHandler('mothers-filtered.csv', content = function(file) {
        s = input$x2_rows_all
        write.csv(testMothers[s, , drop = FALSE], file)
      })
      testFathers <- fathersTable(testFathers, testParents)
      output$x3 = DT::renderDataTable(testFathers, server = FALSE)
      output$x7 = downloadHandler('fathers-filtered.csv', content = function(file) {
        s = input$x3_rows_all
        write.csv(testFathers[s, , drop = FALSE], file)
      })
      # twoDtable <- matrix(c("NA"), nrow=length(testMothers$BrCrCode), ncol=length(testFathers$BrCrCode))
      # rownames(twoDtable) <- testMothers$BrCrCode
      # colnames(twoDtable) <- testFathers$BrCrCode
      # l <- 1
      # m <- 0
      # for(i in 1:length(testParents$BrCrCode)){
      #   motherCodeFound <- 0
      #   fatherCodeFound <- 0
      #   motherCodeFoundString <- ""
      #   fatherCodeFoundString <- ""
      #   for(j in 1:length(testMothers$BrCrCode)){
      #     if(grepl(testMothers$BrCrCode[j], testParents$MotherCode[l])){
      #       motherCodeFound <- j
      #       motherCodeFoundString <- testMothers$BrCrCode[j]
      #       break
      #     }
      #   }
      #   for(k in 1:length(testFathers$BrCrCode)){
      #     if(grepl(testFathers$BrCrCode[k], testParents$FatherCode[l])){
      #       fatherCodeFound <- k
      #       fatherCodeFoundString <- testFathers$BrCrCode[k]
      #       m <- m + 1
      #       break
      #     }
      #   }
      #   for(n in 1:length(testParentsVerbaitum$BrCrCode)){
      #     if(grepl(testParentsVerbaitum$BrCrCode[n], testParents$BrCrCode[i])){
      #       ifelse(testParentsVerbaitum$survivalrate[n] < 40, (twoDtable[motherCodeFound, fatherCodeFound] = paste(testParentsVerbaitum$survivalrate[n], "STS", sep = " ")), (twoDtable[motherCodeFound, fatherCodeFound] = testParentsVerbaitum$survivalrate[n]))
      #       break;
      #     }
      #   }
      #   l <- l + 1
      # }
      # twoDtableDF <- data.frame(twoDtable)
      # return()
    }
  })
  
  #
  # 
  # 
  # 
  # 
  # 
  # output$x4 = DT::renderDataTable(twoDtableDF, server = FALSE)
  # 

  # 

  # 
  # output$x8 = downloadHandler('overall2dmatrix.csv', content = function(file) {
  #   s = input$x4_rows_all
  #   write.csv(twoDtableDF[s, , drop = FALSE], file)
  # })
})