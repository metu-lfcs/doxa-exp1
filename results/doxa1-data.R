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
Expected <- factor(character(0), c(as.character(unique(data$responses)),"None"))
Accuracy <- factor(integer(0), 0:1)
RT <- double(0)

main <- data.frame(Participant,Name,Group,Trial_index,Trial_type,Trial_id,RT,Response,Expected,Accuracy)

#,,RT,Response,Accuracy)

#data <- data[c(7,9,10,4,12,1,13,14,15,16)]

for (i in 1:nrow(data)){

	type <- switch(substr(data[i,"id"], 1, 1),
		F = "filler",
		C = "critical",
		Q = "question",
		A = "arithmetic")

	if (type == "critical") {

		if (data[i,"group"]=="z"){
			expected <- switch(data[i,"id"],
				C01 = "Üzülür", C02 = "Sevinir", C03 = "Üzülür", C04 = "Sevinir", C05 = "Üzülür", C06 = "Sevinir")
		} else if (data[i,"group"]=="b"){
			expected <- switch(data[i,"id"],
				C01 = "Sevinir", C02 = "Üzülür", C03 = "Sevinir", C04 = "Üzülür", C05 = "Sevinir", C06 = "Üzülür")
		} else if (data[i,"group"]=="d"){
			expected <- switch(data[i,"id"],
				C01 = "Bilemeyiz", C02 = "Bilemeyiz", C03 = "Bilemeyiz", C04 = "Bilemeyiz", C05 = "Bilemeyiz", C06 = "Bilemeyiz")
		}
		} else if (type == "filler") {
			expected <- switch(data[i,"id"],
				F07 = "Üzülür", F08 = "Bilemeyiz", F09 = "Bilemeyiz", F10 = "Sevinir", F11 = "Üzülür", F12 = "Bilemeyiz", F13 = "Bilemeyiz", F14 = "Bilemeyiz", F15 = "Bilemeyiz", F16 = "Sevinir", F17 = "Bilemeyiz", F18 = "Sevinir", F19 = "Üzülür", F20 = "Bilemeyiz", F21 = "Üzülür", F22 = "Sevinir", F23 = "Üzülür", F24 = "Sevinir",)
		} else {
			expected <- "None"	
		}	


	if (data[i,"correct"] == "null"){
		accuracy <- NA
	} else if (data[i,"correct"] == data[i,"responses"]) {
		accuracy <- 1
	} else {
		accuracy <- 0
	}

	main[nrow(main)+1, ] <- c(data[i,7],data[i,9],data[i,10],data[i,4],type,data[i,"id"],data[i,1],data[i,14],expected,accuracy)
	
	} 

write.csv(main, file="main-data.csv",row.names=FALSE)
resp <- main %>% filter(Trial_type == "critical") %>% group_by(Name,Group,Response) %>% summarise(Count=n())

write.csv(resp,file="participant-responsetypes.csv", row.names=FALSE)





