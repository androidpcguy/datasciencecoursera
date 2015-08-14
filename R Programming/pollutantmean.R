pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
	        ## 'directory' is a character vector of length 1 indicating
	        ## the location of the CSV files
		
		csvs <- paste(directory, "", sep = "/")
		## print(csvs) TESTING





	        ## 'pollutant' is a character vector of length 1 indicating
	        ## the name of the pollutant for which we will calculate the
	        ## mean; either "sulfate" or "nitrate".
	        
		## 'id' is an integer vector indicating the monitor ID numbers
	        ## to be used
	        file_names <- formatC(id, width = 3, flag = "0")
		datas <- list()
		good_data <- list()
		index <- 1
		for(file_name in file_names) {
			##print(paste("FILE_NAME",file_name,sep = " " ))
			d <- read.csv(paste(csvs, file_name,".csv", sep = "" ))
			datas[[index]] <- subset(d,select = pollutant)
			##print(pol) ##TESTING
			index <- index + 1
		}
		
		for(i in 1:length(datas)) {
			badData <- is.na(datas[[i]])
			good_data[[i]] <- datas[[i]][!badData]
		}
		print(length(good_data)) ##TESTING
		total <- 0
		total_length <- 0

		for(j in 1:length(good_data)) {
			total_length <- total_length + length(good_data[[j]])
			total <- total + sum(good_data[[j]])
		}

		total / total_length
		## Return the mean of the pollutant across all monitors list
	        ## in the 'id' vector (ignoring NA values)
	        ## NOTE: Do not round the result!
	
}
