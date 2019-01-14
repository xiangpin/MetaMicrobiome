#' @title model training 
#'
#' @description
#' build a model with training datasets.
#' @details
#' TODO
#' 
#' @param train_data list or dataframe, the dataframe contianed the features or a list 
#' contianed multi-dataframe with features.
#' @param study character,if train_data is a list, we can use `study` extract 
#' the dataframe, default is NULL.
#' @param numtree numeric, the number of trees for randomforest, see \code{[randomForest]} details.
#' @param number_try numeric, the number of variables randomly sampled as candidates at each split, 
#' see \code{[randomForest]} details, default is `round(sqrt(ncol(train_data)))`.
#' @param numbercv numeric, the number of cross-validation.
#' @param classvariable character, the header name of the train dataframe for the classification.
#'
#' @return a model object, see \code{[randomForest]} details.
#' @author Shuangbin Xu
#' @export
#' @importFrom caret trainControl twoClassSummary train 
#' @examples
#'
#'
#'
#'
make_RF_model <- function(train_data=NULL,
			     study=NULL, 
			     numtree=500, 
			     number_try=NULL, 
			     numbercv=10, 
			     classvariable=NULL){
    	if (!is.null(study)){
	    if (study %in% names(train_data)){
		train_data <- train_data[[study]][["traindata"]]
	    }else{
	    	train_data <- train_data 
	    }
	}else{
		train_data <- train_data 
	}
       fitControl <- caret::trainControl(
       method = "cv",
       number = numbercv, 
       classProbs = TRUE,
	summaryFunction=caret::twoClassSummary,
       savePredictions = "final")
	if (is.null(number_try)){
       	number_try <- round(sqrt(ncol(train_data)))
	}
       tunegrid <- expand.grid(.mtry = number_try)
       set.seed(1000)                  
	formulatmp <- as.formula(paste(classvariable, ".", sep=" ~ "))
       training_model <-  caret::train(formulatmp, data = train_data,
              method = "rf",
              ntree = numtree,
              trControl = fitControl,
              tuneGrid = tunegrid,
              metric = "ROC",
              na.action = na.omit,
              verbose = FALSE)
       return(training_model)
                   
}

#' @title models results
#' 
#' @description
#' Predict the training datasets for a model
#' @details
#' TODO
#' @param model object, a model object. 
#' @param study character, a names of training datasets.
#' @param minus logical, whether minus the part of the `study`, default is FALSE.
#' @param classtype character, the name of the positive group for classificaion.
#' 
#' @return a dataframe for the roc curve plot.
#' @author Shuangbin Xu
#' @export
#' @importFrom pROC roc
#' @examples
#'
#'
#'


predict_train <- function(model=NULL, 
			     study=NULL, 
			     minus=FALSE, 
			     classtype=NULL){
	modelpred <- model$pred
	rocsenspe <- pROC::roc(modelpred$obs, modelpred[,classtype])
	roccurve <- data.frame(cbind(Sensitivity=rocsenspe$sensitivities, Specificity=rocsenspe$specificities))
	roccurve$AUC <- rocsenspe$auc 
	if (!is.null(study)) {
	    	if(isTRUE(minus)){
			roccurve$group <- paste("minus ", study," AUC:", round(rocsenspe$auc,4), sep="")
			roccurve$Train <- paste("minus ", study, sep="")
		}else{
			roccurve$group <- paste(study," AUC:", round(rocsenspe$auc,4), sep="")
			roccurve$Train <- study 
		}
	}else{
		roccurve$group <- paste("Traning cohort AUC:", round(rocsenspe$auc,4), sep="")
	}
	return(roccurve)

}
