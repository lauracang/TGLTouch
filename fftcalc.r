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
	if (grepl("free", x)) {
		gesture <- "free"
	}
	
	return(gesture)
}

cellCol <- function(x) {
	colCount <- seq(1, x, 1)
	for(i in 1:x) {
		colCount[i] <- paste("cell", colCount[i], sep="")
	}
	colNames <- c("timestamp", colCount)
	return(colNames)
}

getFileNameStub <- function(x) {
	fileNameEnd <- as.numeric(regexpr(".txt", x)) - 1
	return(substr(x, 1, fileNameEnd))
}

userID <- c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35")

rootDir <- "/ubc/cs/research/imager/project/spin/people/cang,laura/TGL/TGL logs/data/"
newDir <- "/ubc/cs/research/imager/project/spin/people/cang,laura/TGL/TGL logs/data/mvfft/"

for(m in 3:length(userID)) {
	currDir <- paste(rootDir, userID[m], sep="")
	setwd(currDir)
	print(paste("Processing user ", m, " in ", length(userID), sep=""))
	touchFiles <- list.files(pattern="Touch")
	emotionFiles <- 0 
	for(t in 1:length(touchFiles)) {
		if(grepl("gesture_free", touchFiles[t])) {
			emotionFiles <- c(emotionFiles, touchFiles[t])
		}
	}
	emotionFiles <- emotionFiles[-1]
	for(t in 1:length(emotionFiles)){
		setwd(currDir)
		print(paste("Processing file: ", emotionFiles[t], " which is ", t, " in ", length(emotionFiles), sep=""))
		newFileName <- paste(getFileNameStub(emotionFiles[t]), "mvfft.csv", sep="")
		thisFile <- read.table(emotionFiles[t], header=FALSE)
		# thisTime is the timestamp vector
		thisTime <- thisFile[,1]
		# thisFile is a matrix of touch data
		thisFile <- thisFile[,-1]
		thisFile <- as.matrix(thisFile)
		thisMvfft <- mvfft(thisFile)
		fullMvfft <- cbind(thisTime, thisMvfft)
		colnames(fullMvfft) <- cellCol(ncol(thisMvfft))
		setwd(newDir)
		write.csv(fullMvfft, newFileName, row.names=FALSE)
	}
}