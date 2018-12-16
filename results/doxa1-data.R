#!/usr/bin/Rscript

library(dplyr)

system("rm raw-data.csv")
system("mergeCsv.sh raw-data.csv raw-data/*.csv")
system("rm main-data.csv")

data <- read.csv("raw-data.csv", stringsAsFactors=FALSE)
data <- filter(data, !(trial_type %in% c("external-html","instructions")))
data$responses <- gsub("[}{\"Q0:]","",data$responses)

#data <- mutate(data, accuracy = ifelse(test=(correct==responses),yes=1 , no=0))

# Create factors

Participant <- factor(character(0), as.character(unique(data$subject)))
Name <- factor(character(0), as.character(unique(data$subject_name)))
Group <- factor(character(0), as.character(unique(data$group)))
Trial_index <- factor(integer(0), as.integer(unique(data$trial_index)))
Trial_type <- factor(character(0), c("filler","critical","question","arithmetic"))
Trial_id <- factor(character(0), as.character(unique(data$id))) 
Response <- factor(character(0), as.character(unique(data$responses)))
Accuracy <- factor(integer(0), 0:1)
RT <- double(0)

main <- data.frame(Participant,Name,Group,Trial_index,Trial_type,Trial_id,RT,Response,Accuracy)

#,,RT,Response,Accuracy)

#data <- data[c(7,9,10,4,12,1,13,14,15,16)]

for (i in 1:nrow(data)){

	type <- switch(substr(data[i,"id"], 1, 1),
		F = "filler",
		C = "critical",
		Q = "question",
		A = "arithmetic")

	if (data[i,"correct"] == "null"){
		accuracy <- NA
	} else if (data[i,"correct"] == data[i,"responses"]) {
		accuracy <- 1
	} else {
		accuracy <- 0
	}


	main[nrow(main)+1, ] <- c(data[i,7],data[i,9],data[i,10],data[i,4],type,data[i,"id"],data[i,1],data[i,14],accuracy)
	
	} 


write.csv(main, file="main-data.csv",row.names=FALSE)



	

