#!/usr/bin/Rscript

#source("../R/high_low_vector.R")
#source("../R/thresholds.R")
#source("../R/Run_Risk_measure.R")
#source("../R/RunPooled.R")
library(MetaMicrobiome)

study <- c("Baxter_16", "Deng_18", "Flemer_17", "Flemer_18", "Hale_17", "Mori_18", "Zeller_15")

get_data <- function(study){
	data <- read.csv(paste("../data/",study,"_alpha_data.csv", sep=""))
	return(data)
	
}

data <- mapply(get_data, study, SIMPLIFY=F)
#print(data)

threholds <- mapply(thresholds, data, 
	MoreArgs=list(var_of_interest=c("Observe", "Shannon", "J"), 
			type="median"), SIMPLIFY=F)
print(threholds)
multiHighLowVector <- mapply(MultiHighLow, 
				 data,
				 threholds,
				 MoreArgs=list(var_of_interest=c("Observe", "Shannon", "J")),SIMPLIFY=F)
print(multiHighLowVector)

multiRRresult <- mapply(multiRunRR, 
			   multiHighLowVector, 
			   data, 
			   MoreArgs=list(prefix="Group", grouptype="CRC"), 
			   SIMPLIFY=F)

#print(multiRRresult)

multistudyRRresult <- mapply(multiVarRRTab, 
				 multiRRresult, 
				 MoreArgs=list(var_of_interest=c("Observe", "Shannon", "J")), 
				 SIMPLIFY=F)

multistudyRRresult2 <- dplyr::bind_rows(lapply(study, 
						     function(x) dplyr::mutate(multistudyRRresult[[x]], study=x)))
print(multistudyRRresult2)

pooledREML <- dplyr::bind_rows(mapply(RunPoolEffect, 
					   c("Observe", "Shannon", "J"), 
					   MoreArgs=list(dataset=multistudyRRresult2, 
							   methodtype="REML"), 
					   SIMPLIFY=F))
print(pooledREML)

