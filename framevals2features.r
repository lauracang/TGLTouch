###################################################################################
#	this set of fxns is file name processing									  #
###################################################################################
getEmotion<-function(x) {
	emotion <- "error"
	if (grepl("depressed", x)) {
		emotion <- "depressed"
	}
	if (grepl("excited", x)) {
		emotion <- "excited"
	}
	if (grepl("stressed", x)) {
		emotion <- "stressed"
	}
	if (grepl("relaxed", x)) {
		emotion <- "relaxed"
	}
	return(emotion)
}

#get gesture performed
getGesture<-function(x) {
	gesture <- "error"
	if (grepl("stroke", x)) {
		gesture <- "stroke"
	} 
	if (grepl("pat", x)) {
		gesture <- "pat"
	} 
	if (grepl("rub", x)){
		gesture <- "rub"
	} 
	if (grepl("constant", x)) {
		gesture <- "constant"
	} 
	if (grepl("notouch", x)) {
		gesture <- "notouch"
	} 
	if (grepl("tickle", x)) {
		gesture <- "tickle"
	} 
	if (grepl("scratch", x)) {
		gesture <- "scratch"
	} 
	
	return(gesture)
}

#get the file recording time to sync touch and gaze
getFileTime <- function(x) {
# finds the string that indicates the file
	
	timeLocStart <- as.numeric(regexpr("2015.", x))
	timeLocEnd <- as.numeric(regexpr("-[0-9]+-[A-Z]",x)) - 1
	# subtract 1 for timeLocEndthisFile bc offby1
	fileTime <- substr(x, timeLocStart, timeLocEnd)
	return(fileTime)
}

getFileNameStub <- function(x) {
	fileNameEnd <- as.numeric(regexpr(".csv", x)) - 1
	return(substr(x, 1, fileNameEnd))
}

getSubjectLbl<-function(x) {
	subLoc <- as.numeric(regexpr("[0-9]+-[A-Z]", x))
	#check if participant number is single digit (i.e. P1)
	if (grepl("-", substr(x, subLoc+1, subLoc+1))) {
		subNum <- substr(x, subLoc, subLoc)
		subLbl <- paste("P", subNum, sep="")
	}
	#check if participant number is double digit (i.e. P11)
	if (grepl("[0|1]", substr(x, subLoc+1, subLoc+1))) {
		subNum <- substr(x, subLoc, subLoc+1)
		subLbl <- paste("P", subNum, sep="")
	}
	return(subLbl)
}

getSubjectNum <- function(x) {
	subLoc <- as.numeric(regexpr("[0-9]+-[A-Z]", x))
	#check if participant number is single digit (i.e. P1)
	if (grepl("-", substr(x, subLoc+1, subLoc+1))) {
		subNum <- substr(x, subLoc, subLoc)
	}
	#check if participant number is double digit (i.e. P11)
	if (grepl("[0-9]", substr(x, subLoc+1, subLoc+1))) {
		subNum <- substr(x, subLoc, subLoc+1)
	}
	return(as.numeric(subNum))
}
###################################################################################
#	this set of fxns is statistical feature calcs								  #
###################################################################################
totalvar<-function(x) {
	tmptv<-0
	for (i in 1:(length(x)-1)) {	
		tmp<-abs(x[i] - x[i+1])
		tmptv<-tmptv+tmp
	}
	return(tmptv)
}

auc<-function(x) {
	return(sum(x))
}

findtuple<-function(x) {
	foundmax<-max(x)
	foundmin<-min(x)
	foundmean<-mean(x)
	foundmedian<-median(x)
	foundvariance<-var(x)
	foundtotvar<-totalvar(x)
	foundauc<-auc(x)
	tuple<-c(foundmax, foundmin, foundmean, foundmedian, foundvariance, foundtotvar, foundauc)
	return(tuple)
}

windowFeatures <- c("max", "min", "mean", "median", "var", "totalvar", "auc", "Xmax", "Xmin", "Xmean", "Xmedian", "Xvar", "Xtotalvar", "Xauc", "Ymax", "Ymin", "Ymean", "Ymedian", "Yvar", "Ytotalvar", "Yauc", "subject", "gender", "pet", "age",  "firstLang", "secondLang", "type")

genderList <- c("male", "female", "male", "female", "female", "male", "female", "female", "male", "male", "female", "male", "male", "female", "male", "male", "male", "male", "male", "male", "male", "male", "male", "female", "male", "female", "female", "female", "female", "female", "female", "male", "female", "female", "female")
petList <- c("pet1", "pet1", "pet5", "pet5", "pet1", "pet2", "pet4", "pet3", "pet3", "pet5", "pet1", "pet3", "pet4", "pet1", "pet3", "pet4", "pet2", "pet2", "pet1", "pet3", "pet2", "pet1", "pet3", "pet1", "pet1", "pet1", "pet2", "pet1", "pet2", "pet1", "pet1", "pet2", "pet5", "pet5", "pet1")

ageList <- c("age31", "age29", "age22", "age26", "age21", "age31", "age27", "age25", "age33", "age21", "age24", "age21", "age30", "age25", "age30", "age28", "age25", "age23", "age25", "age24", "age44", "age48", "age29", "age20", "age30", "age19", "age32", "age19", "age19", "age24", "age24", "age24", "age21", "age19", "age24")
langList1 <- c("english", "german", "tamil", "english", "english", "farsi", "english", "english", "english", "english", "farsi", "english", "spanish", "mandarin", "hindi", "spanish", "english", "hindi", "mandarin", "tamil", "english", "romanian", "bengali", "english", "english", "english", "english", "english", "english", "english", "english", "english", "turkish", "english", "english")
langList2 <- c("mandarin", "english", "english", "spanish", "chinese", "english", "german", "french", "russian", "none", "english", "telugu", "english", "english", "english", "english", "hindi", "english", "english", "english", "german", "english", "english", "none", "malayalam", "cantonese", "japanese", "cantonese", "spanish", "spanish", "cantonese", "urdu", "english", "mandarin", "cantonese")

fileGesture<-c("constant", "pat", "rub", "scratch", "stroke", "tickle", "notouch")

fileUserID<-c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35")

newDir<-"C:/Users/Laura/Desktop/TGL data/summary/summary/"
rootDir <- "C:/Users/Laura/Desktop/TGL data/summary/emotion/"

setwd(rootDir)
gestureFiles <- list.files()
# gazeFiles <- list.files(pattern="^GazeLogfile.*")
featureData <- seq(along.with = windowFeatures)

for (j in 1:length(gestureFiles)) {
			
	print(paste("Processing ", gestureFiles[j], "which is: ", j, "of 229", sep=""))
	
	
	thisSubject <- getSubjectNum(gestureFiles[j])
	thisSubName <- fileUserID[thisSubject]
	thisGender <- genderList[thisSubject]
	thisPetFreq <- petList[thisSubject]
	thisAge <- ageList[thisSubject]
	thisLang1<-langList1[thisSubject]
	thisLang2<-langList2[thisSubject]
	thisGesture <- getGesture(gestureFiles[j])
	
	thisTouchdata <- read.csv(gestureFiles[j], header=TRUE) 		
	
	timedata <- thisTouchdata$timestamp
	framesums <- thisTouchdata$framesum
	xlocations <- thisTouchdata$x.loc
	ylocations <- thisTouchdata$y.loc
	
	window1fs <- -1
	window1x <- -1 
	window1y <- -1 
	window2fs <- -1
	window2x <- -1 
	window2y <- -1 
	window3fs <- -1
	window3x <- -1 
	window3y <- -1 
	window4fs <- -1
	window4x <- -1 
	window4y <- -1 
	
	for (n in 1:nrow(thisTouchdata)) {
	
		# ignore first and last second of collected data
		if (timedata[n] >= 1000 && timedata[n] < 3000) {
			window1fs<-c(window1fs, framesums[n])
			window1x<-c(window1x, xlocations[n])
			window1y<-c(window1y, ylocations[n])
		}
		if (timedata[n] >= 3000 && timedata[n] < 5000) {
			window2fs<-c(window2fs, framesums[n])
			window2x<-c(window2x, xlocations[n])
			window2y<-c(window2y, ylocations[n])
		}
		if (timedata[n] >= 5000 && timedata[n] < 7000) {
			window3fs<-c(window3fs, framesums[n])
			window3x<-c(window3x, xlocations[n])
			window3y<-c(window3y, ylocations[n])
		}
		if (timedata[n] >= 7000 && timedata[n] < 9000) {
			window4fs<-c(window4fs, framesums[n])
			window4x<-c(window4x, xlocations[n])
			window4y<-c(window4y, ylocations[n])
		}
		else {}
	}
	
	window1fs <- window1fs[-1]
	window1x <- window1x[-1]
	window1y <- window1y[-1]
	window2fs <- window2fs[-1]
	window2x <- window2x[-1]
	window2y <- window2y[-1]
	window3fs <- window3fs[-1]
	window3x <- window3x[-1]
	window3y <- window3y[-1]
	window4fs <- window4fs[-1]
	window4x <- window4x[-1]
	window4y <- window4y[-1]

	window1 <- c(findtuple(window1fs), findtuple(window1x), findtuple(window1y), thisSubName, thisGender, thisPetFreq, thisAge, thisLang1, thisLang2, thisGesture)
	window2 <- c(findtuple(window2fs), findtuple(window2x), findtuple(window2y), thisSubName, thisGender, thisPetFreq, thisAge, thisLang1, thisLang2, thisGesture)
	window3 <- c(findtuple(window3fs), findtuple(window3x), findtuple(window3y), thisSubName, thisGender, thisPetFreq, thisAge, thisLang1, thisLang2, thisGesture)
	window4 <- c(findtuple(window4fs), findtuple(window4x), findtuple(window4y), thisSubName, thisGender, thisPetFreq, thisAge, thisLang1, thisLang2, thisGesture)
	
	#windowFeatures <- c("max", "min", "mean", "median", "var", "totalvar", "auc", "Xmax", "Xmin", "Xmean", "Xmedian", "Xvar", "Xtotalvar", "Xauc", "Ymax", "Ymin", "Ymean", "Ymedian", "Yvar", "Ytotalvar", "Yauc", "subject", "gender", "pet", "age",  "firstLang", "secondLang", "type")
	
	featureData <- rbind(featureData, window1, window2, window3, window4)
	
}
setwd(newDir)
featureData<-featureData[-1,]	
colnames(featureData) <- windowFeatures
write.csv(featureData, file="gesturefeaturefile.csv", row.names=FALSE)