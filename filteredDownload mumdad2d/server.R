

shinyServer(function(input, output, session) {
  
  ## Access file as input$upload_file$datapath inside a reactive element in server.R
  testParents <- mutate(select(fread(file.choose(), header = TRUE, data.table = FALSE), BrCrCode, Planted, PsaDeaths), MotherCode = '', FatherCode = '')
  slashCount <- str_count(testParents$BrCrCode, '/')
  underscoreCount <- str_count(testParents$BrCrCode, '_')
  spaceCheck <- str_count(testParents$BrCrCode, ' ')
  overallSurvival <- -(1 - ((sum(as.numeric(testParents$PsaDeaths)) / sum(as.numeric(testParents$Planted))) * 100))
  print(overallSurvival)
  for(i in 1:(dim(testParents)[1])){
    if(spaceCheck[i]==1){
      testParents$BrCrCode[i] <- gsub(" ", "_", testParents$BrCrCode[i])
      underscoreCount[i] = 1
    }
    if(testParents$BrCrCode[i]=="") {
      testParents$BrCrCode[i] <- paste("PH_", paste(i, "ph", sep = ""), sep = "")
      testParents$MotherCode[i] <- "PH"
      testParents$FatherCode[i] <- paste(i, "ph", sep = "")
    }
    if(slashCount[i]==1) {
      testParents$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1]
      testParents$MotherCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1]
      testParents$FatherCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[2]
    }
    if(slashCount[i]==3) {
      testParents$BrCrCode[i] <- paste("control_", unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], sep = "")
      testParents$MotherCode[i] <- "control"
      testParents$FatherCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1]
    }
    if(underscoreCount[i] == 0) {
      testParents$BrCrCode[i] <- paste(as.character(str_extract(testParents$BrCrCode[i], "[aA-zZ]+")), "_", as.character(str_extract(testParents$BrCrCode[i], "[0-9]+")), sep = "")
      testParents$MotherCode[i] <- as.character(str_extract(testParents$BrCrCode[i], "[aA-zZ]+"))
      testParents$FatherCode[i] <- as.character(str_extract(testParents$BrCrCode[i], "[0-9]+"))
    }
    #ifelse(testParents$BrCrCode[i]=="", testParents$BrCrCode[i] <- paste("PH_", paste(i, "ph", sep = ""), sep = ""), ifelse(slashCount[i]==1, testParents$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], ifelse((slashCount[i]==3), testParents$BrCrCode[i] <- paste("control_", unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], sep = ""), testParents$BrCrCode[i])))
    #ifelse(underscoreCount[i] == 0, (testParents$BrCrCode[i] <- paste(as.character(str_extract(testParents$BrCrCode[i], "[aA-zZ]+")), "_", as.character(str_extract(testParents$BrCrCode[i], "[0-9]+")), sep = "")), "")
  }
  testParents$Planted <- as.numeric(testParents$Planted)
  testParents$PsaDeaths <- as.numeric(testParents$PsaDeaths)
  #find more suitable location to calculate a verbatium table?
  testParents$survivalrate <- format(round((value = 100 - ((testParents$PsaDeaths / testParents$Planted) * 100)), 2), nsmall = 2)
  testMothers <- testFathers <- testParentsVerbaitum <- testParents <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode+MotherCode+FatherCode+survivalrate, data = testParents, FUN = sum)
  testParentsVerbaitum$MotherCode <- NULL
  testParentsVerbaitum$FatherCode <- NULL
  testParentsVerbaitum$survivalrate <- NULL
  testParentsVerbaitum <- mutate(aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testParentsVerbaitum, FUN = sum), survivalrate = round((100 - (PsaDeaths / Planted) * 100), 2))
  underscoreCount <- str_count(testParents$BrCrCode, '_')
  for(i in 1:(dim(testParents))){
    if(underscoreCount[i]==2){
      testParents$MotherCode[i] <- testMothers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[1]
      testParents$FatherCode[i] <- testFathers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[3]
      next
    }
    testParents$MotherCode[i] <- testMothers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[1]
    testParents$FatherCode[i] <- testFathers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[2]
  }
  nums <- sapply(testMothers, is.numeric)
  testMothers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testMothers, FUN = sum)
  testFathers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testFathers, FUN = sum)
  testMothers <- mutate(testMothers, survivalrate = round((100 - (PsaDeaths / Planted) * 100), 2))
  testFathers <- mutate(testFathers, survivalrate = round((100 - (PsaDeaths / Planted) * 100), 2))
  twoDtable <- matrix(c("NA"), nrow=length(testMothers$BrCrCode), ncol=length(testFathers$BrCrCode))
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
        ifelse(testParentsVerbaitum$survivalrate[n] > 40, (twoDtable[motherCodeFound, fatherCodeFound] = paste(testParentsVerbaitum$survivalrate[n], "STS", sep = " ")), (twoDtable[motherCodeFound, fatherCodeFound] = testParentsVerbaitum$survivalrate[n]))
        break;
      }
    }
    l <- l + 1
  }
  twoDtableDF <- data.frame(twoDtable)
  
  print(twoDtable)
  
  #
    
  output$x1 = DT::renderDataTable(testParents, server = FALSE)
  
  output$x2 = DT::renderDataTable(testMothers, server = FALSE)
  
  output$x3 = DT::renderDataTable(testFathers, server = FALSE)
  
  output$x4 = DT::renderDataTable(twoDtableDF, server = FALSE)
  
  # download the filtered data
  output$x5 = downloadHandler('parents-filtered.csv', content = function(file) {
    s = input$x1_rows_all
    write.csv(testParents[s, , drop = FALSE], file)
  })
  
  output$x6 = downloadHandler('mothers-filtered.csv', content = function(file) {
    s = input$x2_rows_all
    write.csv(testMothers[s, , drop = FALSE], file)
  })
  
  output$x7 = downloadHandler('fathers-filtered.csv', content = function(file) {
    s = input$x3_rows_all
    write.csv(testFathers[s, , drop = FALSE], file)
  })
  
  output$x8 = downloadHandler('overall2dmatrix.csv', content = function(file) {
    s = input$x4_rows_all
    write.csv(twoDtableDF[s, , drop = FALSE], file)
  })
})