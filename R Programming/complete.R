complete <- function(directory, id = 1:332) {
	csvs <- paste(directory, "", sep = "/")

	file_names <- formatC(id, width = 3, flag = "0")
	
	data_frames <- list()
	nobs <- list()
	for(i in 1:length(id)) {
		d <- read.csv(paste(csvs,file_names[[i]],".csv",sep=""))
		data_frames[[i]] <- na.omit(d)
		nobs[i] <- nrow(data_frames[[i]])
	}
	## TESTING: print(data_frames[[1]])	
	data.frame(cbind(id,nobs))


}
