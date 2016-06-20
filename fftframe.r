getFileNameStub <- function(x) {
	fileNameEnd <- as.numeric(regexpr(".csv", x)) - 1
	return(substr(x, 1, fileNameEnd))
}

emotionDir <- "/ubc/cs/research/imager/project/spin/people/cang,laura/TGL/TGL logs/data summary/emotion/"
mvfftDir <- "/ubc/cs/research/imager/project/spin/people/cang,laura/TGL/TGL logs/data/mvfft/"
framemvfftDir <- "/ubc/cs/research/imager/project/spin/people/cang,laura/TGL/TGL logs/data summary/emotionmvfft/"

# for each mvfft file find the peak pixel and the value
setwd(mvfftDir)
mvfftFilesAll <- list.files()
setwd(emotionDir)
emotionFiles <- list.files()
newEmoteSet <- 0
newMvfftSet <- 0

mvfftFiles <- 0
for (i in 1:length(mvfftFilesAll)) {
	if(!grepl("emotion_free", mvfftFilesAll[i])) {
		mvfftFiles <- c(mvfftFiles, mvfftFilesAll[i])
	}
}
mvfftFiles <- mvfftFiles[-1]

for(m in 28:length(emotionFiles)) {
	emoteFile <- getFileNameStub(emotionFiles[m])
	mvfftFile <- getFileNameStub(mvfftFiles[m])
	if(grepl(emoteFile,mvfftFile)){		
		setwd(mvfftDir)
		print(paste("Processing file ", mvfftFiles[m], " which is ", m, " out of ", length(mvfftFiles), sep=""))
		mvfftName <- paste(getFileNameStub(mvfftFiles[m]), "fft.csv", sep="")
		thisMvfft <- read.csv(mvfftFiles[m], header=TRUE)
		peak <- c(0,0)
		for(r in 1:nrow(thisMvfft)) {
			thisRow <- thisMvfft[r,-1]
			thisNumRow <- as.numeric(thisRow)
			maxFreq <- max(thisNumRow)
			maxIndex <- which.max(thisNumRow)
			newMax <- c(maxIndex, maxFreq)
			peak <- rbind(peak, newMax)
		}
		peak <- peak[-1,]
		colnames(peak) <- c("peak cell", "peak freq")
		setwd(emotionDir)
		thisEmotion <- read.csv(emotionFiles[m], header=TRUE)
		newFileHere <- cbind(thisEmotion,peak)
		setwd(framemvfftDir)
		write.csv(newFileHere, file=mvfftName, row.names=FALSE)
	}
	else{
		print("Files don't match: ", emotionFiles[m], " and ", mvfftFiles[m], " which is ", m, " out of ", length(emotionFiles), sep="")
		newEmoteSet <- c(newEmoteSet, emotionFiles[m])
		newMvfftSet <- c(newMvfftSet, mvfftFiles[m])
	}
	
}


# read the corresponding emotion file and cbind framedmvfft
# write to framemvfftDir