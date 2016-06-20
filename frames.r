#find the row centroid
centrow<-function(x) {
	centx <- x
	rtotalval <- sum(x)
	if (rtotalval != 0) {
		for (i in 1:9) {
			#takes 10 vals in a row to calculate row centroid
			rowstart <- i*10 + 1
			rowend <- i*10 + 10
			tmpval <- (i+1) * x[rowstart:rowend]
			centx[rowstart:rowend] <- tmpval
		}
		return(sum(centx)/rtotalval)
	}
	else
		return(sum(centx))
}

#find the col centroid
centcol<-function(x) {
	centy <- x
	totalval <- sum(x)
	if (totalval != 0) {
		if (length(x) != 100) {
			print("ERR: wrong frame dim")
		} 
		else {
			for (i in 1:100) {
				column <- i %% 10
				centy[i] <- column * x[i]
			}
		}
		return(sum(centy)/totalval)
	}
	else	
		return(colcent)
}
###################################################################################
#	this set of fxns is file name processing									  #
###################################################################################


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
	if (grepl("gesture_free", x)) {
		gesture <- "free"
	}
	
	return(gesture)
}

#get the block 
#-1- is the one WITH gesture descriptions
#-2- does NOT have gesture descriptions
getBlock<-function(x) {
# finds the 36th character in TouchLogfiles (which is the block #)
# the 35th character is the block in GazeLogfiles
	
	blockLoc <- as.numeric(regexpr("1-[A-Z]", x))
	block <- substr(x, blockLoc, blockLoc)
	return(block)
}

#get the file recording time to sync touch and gaze
getFileTime <- function(x) {
# finds the string that indicates the file
	
	timeLocStart <- as.numeric(regexpr("2015.1[1|2].[0-9]+-", x))
	timeLocEnd <- as.numeric(regexpr("-[0-9]+-[A-Z]+-",x)) - 1
	# subtract 1 for timeLocEnd bc offby1
	fileTime <- substr(x, timeLocStart, timeLocEnd)
	return(fileTime)
}

getFileNameStub <- function(x) {
	fileNameEnd <- as.numeric(regexpr(".txt", x)) - 1
	return(substr(x, 1, fileNameEnd))
}

getSubject<-function(x) {
	subLoc <- as.numeric(regexpr("-[A-Z]+-emotion_", x))
	#check if participant number is single digit (i.e. P1)
	if (grepl("-", substr(x, subLoc-2, subLoc-2))) {
		subNum <- substr(x, subLoc-1, subLoc-1)
		subLbl <- paste("P0", subNum, sep="")
	}
	#check if participant number is double digit (i.e. P11)
	if (grepl("[0|1|2|3]", substr(x, subLoc-2, subLoc-2))) {
		subNum <- substr(x, subLoc-2, subLoc-1)
		subLbl <- paste("P", subNum, sep="")
	}
	return(subLbl)
}

getEmotion<-function(x) {
	emotion <- "error"
	if (grepl("emotion_relaxed", x) && grepl("gesture_free", x)) {
		emotion <- "relaxed"
	} 
	if (grepl("emotion_excited", x) && grepl("gesture_free", x)) {
		emotion <- "excited"
	} 
	if (grepl("emotion_depressed", x) && grepl("gesture_free", x)) {
		emotion <- "depressed"
	} 
	if (grepl("emotion_stressed", x) && grepl("gesture_free", x)) {
		emotion <- "stressed"
	} 
	if (!grepl("gesture_free", x)) {
		emotion <- "free"
	}
	return(emotion)
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



windowFeatures <- c("max", "min", "mean", "median", "var", "totalvar", "auc", "Xmax", "Xmin", "Xmean", "Xmedian", "Xvar", "Xtotalvar", "Xauc", "Ymax", "Ymin", "Ymean", "Ymedian", "Yvar", "Ytotalvar", "Yauc", "subject", "gender", "type")



fileGesture<-c("constant", "pat", "rub", "scratch", "stroke", "tickle", "notouch")
fileUserID<-c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20", "P21", "P22", "P23", "P24", "P25", "P26", "P27", "P28", "P29", "P30", "P31", "P32", "P33", "P34", "P35")

newDir<-"C:/Users/laura/Desktop/TGL logs/summary"
for (m in 1:35) {
	
	userID <- fileUserID[m]
	print(paste("Processing ", userID, sep=""))
	# locate correct directory
	rootDir<-paste("C:/Users/laura/Desktop/TGL logs/data/",userID,"/", sep="")
	setwd(rootDir)
	
	
	#create list of all touchFiles
	touchFiles<-list.files(pattern="*TouchLogfile*")
	gestureFiles <- 0
	for (i in 1: length(touchFiles)) {
		if (getEmotion(touchFiles[i]) == "free") {
			gestureFiles <- c(gestureFiles, touchFiles[i])
		}
	}
	gestureFiles <- gestureFiles[-1]
	#create list of all gazeFiles
	#gazeFiles <- list.files(pattern="^GazeLogfile.*")
	
	# in loop
	#for (n in 1:length(fileGesture)) {
		#gesture <- fileGesture[n]
			
	
	#for (j in 1:length(touchFiles)) {
	for (j in 1:length(gestureFiles)) {
		setwd(rootDir)
		#print(paste("Processing file: ", touchFiles[j], sep=""))
		#print(paste("which is: ", j, " of ", length(touchFiles)))
		print(paste("Processing file: ", gestureFiles[j], sep=""))
		print(paste("which is: ", j, " of ", length(gestureFiles)))			
		touchframevals <- c(0,0,0)
	
		if (m == 1 || m == 2) { # because P01 and P02 have gaze data integrated in touch files
			#alldata <- read.delim(touchFiles[j], header=TRUE)
			alldata <- read.delim(gestureFiles[j], header=TRUE)
			touchdata <- alldata[complete.cases(alldata[, 4:103]),] #gets rid of rows where Touch data is NA
			touchdata <- touchdata[complete.cases(touchdata[, 1]),]
			thisTouchdata <- touchdata[,grepl('^Touch_*', colnames(touchdata))] #gets only cols w 'Touch_'
			thistimestamp <- touchdata$Timestamp
			
		} else {
			#alldata <- read.delim(touchFiles[j], header=FALSE)
			alldata <- read.delim(gestureFiles[j], header=FALSE)
			touchdata <- alldata[complete.cases(alldata[,4:103]),]
			touchdata <- touchdata[complete.cases(alldata[, 1]),]
			thisTouchdata <- touchdata[,-(1:3)]
			thistimestamp <- touchdata[, 1]
		}
					
		for (k in 1:nrow(thisTouchdata)) {
			print(paste("Processing row: ", k, " of ", nrow(thisTouchdata), " in file ", j, " of ", length(gestureFiles), sep=""))
			#print(paste("Processing row: ", k, " of ", nrow(thisTouchdata), " in file ", j, " of ", length(touchFiles), sep=""))
			frametmp <- sum(thisTouchdata[k,])
			colcentroid<-centcol(thisTouchdata[k,])
			rowcentroid<-centrow(thisTouchdata[k,])
				
			rowvals <- c(frametmp, colcentroid, rowcentroid)
			touchframevals <- rbind(touchframevals,rowvals)
			
		}
		touchframevals<-touchframevals[-1,]
		touchframevals<-cbind(thistimestamp, touchframevals)
		colnames(touchframevals)<-c("timestamp", "framesum", "x-loc", "y-loc")
		setwd(newDir)
		#write.csv(touchframevals, file=paste(substr(touchFiles[j], 1, nchar(touchFiles[j])-4), ".csv", sep=""), row.names=FALSE)
		write.csv(touchframevals, file=paste(substr(gestureFiles[j], 1, nchar(gestureFiles[j])-4), ".csv", sep=""), row.names=FALSE)
		
	}		
}