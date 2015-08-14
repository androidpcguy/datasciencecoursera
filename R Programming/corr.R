corr <- function(directory = "specdata", threshold = 0) {

		data_frames <- list()
		
		file_list <- list.files(directory, full.names = TRUE)
		correlations <- numeric()
		index <- 1
		for(i in 1:length(file_list)) {
			d <- read.csv(file_list[[i]])
			data_frames[[i]] <- subset(d, complete.cases(d)) ## na.omit(d)
			if(nrow(data_frames[[i]]) <= threshold) {
				next
			}

			##data_frames[[i]] <- subset(d, !is.na(d$nitrate))
			##data_frames[[i]] <- subset(data_frames[[i]],!is.na(data_frames[[i]]$sulfate))
			correlations[[index]] <- cor(data_frames[[i]]$sulfate, data_frames[[i]]$nitrate, use = "na.or.complete")
			index <- index + 1
		}
		correlations <- na.omit(correlations)
		correlations

}
