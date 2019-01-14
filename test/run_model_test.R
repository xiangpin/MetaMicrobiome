#source("../R/make_RF_model.R")
#source("../R/predict_test.R")
#source("../R/ROCplot.R")
#source("../R/leave_one_out_study.R")
library(MetaMicrobiome)

study <- c("Baxter_16", "Deng_18", "Flemer_17", "Flemer_18", "Zeller_15")

get_genus <- function(study){
	data <- read.csv(paste("../data/", study, "_crc_genera_group.csv", sep=""), header=T, check.names=F)
	return(data)
}

totaldata <- mapply(get_genus, study, SIMPLIFY=F)

#totalmodel <- mapply(make_RF_model,
#		       totaldata,	
#			MoreArgs=list(numtree=400, 
#					numbercv=10, 
#					prefixGroup="Group"), 
#			SIMPLIFY=F)

#predict_test <- mapply(get_mapply_predict_test, 
#			  study, 
#			  MoreArgs=list(dataset=totaldata, 
#					  models=totalmodel, 
#					  classvariable="Group", 
#					  classtype="CRC"), 
#			  SIMPLIFY=F)

#data <- read.csv("../data/Baxter_16_crc_genera_group.csv", header=T, check.names=F)
#testdata <- read.csv("../data/Deng_18_crc_genera_group.csv", header=T, check.names=F)
#datamodel <- make_RF_model(train_data=data, numtree=400, numbercv=10, classvariable="Group")
#print(datamodel)
#rocmodel <- predict_train(datamodel, classtype="CRC")
#print(rocmodel)

#predict_test <- predict_test(datamodel, teststudy="Deng_18", 
#				 Trainstudy="Baxter_16", 
#				 dataset=testdata, 
#				 classvariable="Group", 
#				 classtype="CRC")
#print(predict_test)
traintestdata <- mapply(get_train_test_data, 
			   study, 
			   MoreArgs=list(datasets=totaldata), 
			   SIMPLIFY=F)
train_leaveOneout_model <- mapply(make_RF_model, 
				      study, 
				      MoreArgs=list(train_data=traintestdata, classvariable="Group"),
				      SIMPLIFY=F)

total_model_pred <- dplyr::bind_rows(mapply(predict_train, 
			      study=study, 
			      model=train_leaveOneout_model, 
			      MoreArgs=list(minus=T, classtype="CRC"), 
			      SIMPLIFY=F))
head(total_model_pred)
colors <- c("#E41A1C", 
	    "#4DAF4A", 
           "#984EA3", 
           "#FF7F00", 
           "#F781BF")


minus_p <- ROCplot(rocplotdata=total_model_pred,
	 x="Specificity",
	 y="Sensitivity",
	 xlab="Specificity",
	 ylab="Sensitivity",
	 roccolors=colors,
	 group="group",
	 color="group")
svg("minus_model.svg")
minus_p
dev.off()


