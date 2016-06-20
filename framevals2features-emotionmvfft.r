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

windowFeatures <- c("timestamp", "max", "min", "mean", "median", "var", "totalvar", "auc", "Xmax", "Xmin", "Xmean", "Xmedian", "Xvar", "Xtotalvar", "Xauc", "Ymax", "Ymin", "Ymean", "Ymedian", "Yvar", "Ytotalvar", "Yauc", "Cmax", "Cmin", "Cmean", "Cmedian", "Cvar", "Ctotalvar", "Cauc", "Fmax", "Fmin", "Fmean", "Fmedian", "Fvar", "Ftotalvar", "Fauc","subject", "gender", "pet", "age",  "firstLang", "secondLang", "type")

genderList <- c("male", "female", "male", "female", "female", "male", "female", "female", "male", "male", "female", "male", "male", "female", "male", "male", "male", "male", "male", "male", "male", "male", "male", "female", "male", "female", "female", "female", "female", "female", "female", "male", "female", "female", "female")
petList <- c("pet1", "pet1", "pet5", "pet5", "pet1", "pet2", "pet4", "pet3", "pet3", "pet5", "pet1", "pet3", "pet4", "pet1", "pet3", "pet4", "pet2", "pet2", "pet1", "pet3", "pet2", "pet1", "pet3", "pet1", "pet1", "pet1", "pet2", "pet1", "pet2", "pet1", "pet1", "pet2", "pet5", "pet5", "pet1")

ageList <- c("age31", "age29", "age22", "age26", "age21", "age31", "age27", "age25", "age33", "age21", "age24", "age21", "age30", "age25", "age30", "age28", "age25", "age23", "age25", "age24", "age44", "age48", "age29", "age20", "age30", "age19", "age32", "age19", "age19", "age24", "age24", "age24", "age21", "age19", "age24")
langList1 <- c("english", "german", "tamil", "english", "english", "farsi", "english", "english", "english", "english", "farsi", "english", "spanish", "mandarin", "hindi", "spanish", "english", "hindi", "mandarin", "tamil", "english", "romanian", "bengali", "english", "english", "english", "english", "english", "english", "english", "english", "english", "turkish", "english", "english")
langList2 <- c("mandarin", "english", "english", "spanish", "chinese", "english", "german", "french", "russian", "none", "english", "telugu", "english", "english", "english", "english", "hindi", "english", "english", "english", "german", "english", "english", "none", "malayalam", "cantonese", "japanese", "cantonese", "spanish", "spanish", "cantonese", "urdu", "english", "mandarin", "cantonese")

fileGesture<-c("constant", "pat", "rub", "scratch", "stroke", "tickle", "notouch")

fileUserID<-c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35")

#newDir<-"C:/Users/Laura/Desktop/TGL data/summary/data summary/summary/"
newDir <- "/ubc/cs/research/imager/project/spin/proj/tgl/touch/summary/timestamped/"
#rootDir <- "C:/Users/Laura/Desktop/TGL data/summary/emotionmvfft/"
rootDir <- "/ubc/cs/research/imager/project/spin/people/cang,laura/TGL/TGL logs/data summary/emotionmvfft/"
# REFERS TO POST MVFFT CALCS DIRECTORY

setwd(rootDir)
emotionFiles <- list.files()

featureData <- seq(along.with = windowFeatures)

for (j in 1:length(emotionFiles)) {
			
	print(paste("Processing ", emotionFiles[j], " which is: ", j, " of ", length(emotionFiles), sep=""))
	
	
	thisSubject <- getSubjectNum(emotionFiles[j])
	thisSubName <- paste("P", thisSubject, sep="")
	thisGender <- genderList[thisSubject]
	thisPetFreq <- petList[thisSubject]
	thisAge <- ageList[thisSubject]
	thisLang1<-langList1[thisSubject]
	thisLang2<-langList2[thisSubject]
	thisEmotion <- getEmotion(emotionFiles[j])
	
	thisTouchdata <- read.csv(emotionFiles[j], header=TRUE) 		
	
	timedata <- thisTouchdata$timestamp
	framesums <- thisTouchdata$framesum
	xlocations <- thisTouchdata$x.loc
	ylocations <- thisTouchdata$y.loc
	cpeak <- thisTouchdata[,5]
	fpeak <- thisTouchdata[,6]
	
	lastTime <- timedata[length(timedata)]
	lastWin <- lastTime - 1000
	
	winStart <- 1000
	# 2 sec windows
	#winEnd <- winStart + 2000
	# 1s window
	#winEnd <- winStart + 1000
	# 0.5s window
	winEnd <- winStart + 500
	# 0.2s window
	# winEnd <- winStart + 200
	
	
	while(winEnd < lastWin) {
		wintime <- -1
		winfs <- -1
		winx <- -1
		winy <- -1
		winc <- -1
		winf <- -1
		
		for (n in 1:nrow(thisTouchdata)) {
			if (timedata[n] >= winStart && timedata[n] < winEnd){
				wintime <- c(wintime, timedata[n])
				winfs <- c(winfs, framesums[n])
				winx <- c(winx, xlocations[n])
				winy <- c(winy, ylocations[n])
				winc <- c(winc, cpeak[n])
				winf <- c(winf, fpeak[n])
			}
			else {}
		}
		wintime <- wintime[-1]
		winfs <- winfs[-1]
		winx <- winx[-1]
		winy <- winy[-1]
		winc <- winc[-1]
		winf <- winf[-1]
		
		winCalc <- c(min(wintime), findtuple(winfs), findtuple(winx), findtuple(winy), findtuple(winc), findtuple(winf), thisSubName, thisGender, thisPetFreq, thisAge, thisLang1, thisLang2, thisEmotion)
		
		if(!anyNA(winCalc)) {
			featureData <- rbind(featureData, winCalc)
		}
		# + 1000 to skip 1s in between
		winStart <- winEnd 
		
		# 0.2s window
		winEnd <- winStart + 500
	}
	
}
setwd(newDir)
featureData<-featureData[-1,]	
colnames(featureData) <- windowFeatures
write.csv(featureData, file="05semotionmvfftfeaturefiletimed.csv", row.names=FALSE)
print("file written - program complete")