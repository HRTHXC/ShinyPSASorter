initialTable <- function() {
  library(data.table)
  library(dplyr)
  library(stringr)
  testParents <- select(fread(file.choose(), header = TRUE, data.table = FALSE), BrCrCode, Planted, PsaDeaths)
  for(i in 1:(dim(testParents)[1])){
    slashCount <- str_count(testParents$BrCrCode, '/')
    ifelse(testParents$BrCrCode[i]=="", testParents$BrCrCode[i] <- paste("PH_", i, sep = ""), ifelse(slashCount==1, testParents$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], ifelse(slashCount==3, testParents$BrCrCode[i] <- paste("control_", unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], sep = ""), "")))
    underscoreCount <- str_count(testParents$BrCrCode, '_')
    ifelse(underscoreCount[i] == 0, (testParents$BrCrCode[i] <- paste(as.character(str_extract(testParents$BrCrCode[i], "[aA-zZ]+")), "_", as.numeric(str_extract(testParents$BrCrCode[i], "[0-9]+")), sep = "")), "")
  }
  testParents <- mutate(aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testParents, FUN = sum), survivalrate = (format(round((value = 100 - ((as.numeric(PsaDeaths) / as.numeric(Planted)) * 100)), 2), nsmall = 2)))
  return(testParents)
}

splitAndRecreate <- function() {
  testMothers <- testFathers <- testParents
  for(i in 1:(dim(testParents))){
    testMothers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[1]
    testFathers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[2]
  }
  nums <- sapply(testMothers, is.numeric)
  testMothers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testMothers, FUN = sum)
  testFathers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testFathers, FUN = sum)
  testMothers <- mutate(round(mutate(testMothers[ , nums], survivalrate = (100 - (PsaDeaths / Planted) * 100)), 2), MotherBrCrCode = testMothers$BrCrCode )
  testFathers <- mutate(round(mutate(testFathers[ , nums], survivalrate = (100 - (PsaDeaths / Planted) * 100)), 2), FatherBrCrCode = testFathers$BrCrCode )
  print(testMothers)
  print(testFathers)
}

writetoCSV <- function() {
  write.csv(testParents, file = file.choose(new=TRUE))
  write.csv(testMothers, file = file.choose(new=TRUE))
  write.csv(testFathers, file = file.choose(new=TRUE))
}
