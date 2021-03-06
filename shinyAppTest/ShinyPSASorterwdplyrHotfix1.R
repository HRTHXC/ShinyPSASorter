library(data.table)
library(dplyr)
library(stringr)
testParents <- select(fread(file.choose(), header = TRUE, data.table = FALSE), BrCrCode, Planted, PsaDeaths)
slashCount <- str_count(testParents$BrCrCode, '/')
underscoreCount <- str_count(testParents$BrCrCode, '_')
spaceCheck <- str_count(testParents$BrCrCode, ' ')
for(i in 1:(dim(testParents)[1])){
  if(spaceCheck[i]==1){
    testParents$BrCrCode[i] <- gsub(" ", "_", testParents$BrCrCode[i])
    underscoreCount[i] = 1
  }
  ifelse(testParents$BrCrCode[i]=="", testParents$BrCrCode[i] <- paste("PH_", paste(i, "ph", sep = ""), sep = ""), ifelse(slashCount[i]==1, testParents$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], ifelse((slashCount[i]==3), testParents$BrCrCode[i] <- paste("control_", unlist(strsplit(testParents$BrCrCode[i], split = '/', fixed=TRUE))[1], sep = ""), testParents$BrCrCode[i])))
  ifelse(underscoreCount[i] == 0, (testParents$BrCrCode[i] <- paste(as.character(str_extract(testParents$BrCrCode[i], "[aA-zZ]+")), "_", as.character(str_extract(testParents$BrCrCode[i], "[0-9]+")), sep = "")), "")
}
testParents$Planted <- as.numeric(testParents$Planted)
testParents$PsaDeaths <- as.numeric(testParents$PsaDeaths)
testParents <- mutate(aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testParents, FUN = sum), survivalrate = (format(round((value = 100 - ((PsaDeaths / Planted) * 100)), 2), nsmall = 2)))

testMothers <- testFathers <- testParents
underscoreCount <- str_count(testParents$BrCrCode, '_')
for(i in 1:(dim(testParents))){
  if(underscoreCount[i]==2){
    testMothers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[1]
    testFathers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[3]
    next
  }
  testMothers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[1]
  testFathers$BrCrCode[i] <- unlist(strsplit(testParents$BrCrCode[i], split = '_', fixed=TRUE))[2]
}
nums <- sapply(testMothers, is.numeric)
testMothers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testMothers, FUN = sum)
testFathers <- aggregate(cbind(Planted, PsaDeaths) ~ BrCrCode, data = testFathers, FUN = sum)
testMothers <- mutate(round(mutate(testMothers[ , nums], survivalrate = (100 - (PsaDeaths / Planted) * 100)), 2), MotherBrCrCode = testMothers$BrCrCode )
testFathers <- mutate(round(mutate(testFathers[ , nums], survivalrate = (100 - (PsaDeaths / Planted) * 100)), 2), FatherBrCrCode = testFathers$BrCrCode )
testMothers <- testMothers[c(4,1,2,3)]
testFathers <- testFathers[c(4,1,2,3)]