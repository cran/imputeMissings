#' Impute missing values with the median/mode or \code{randomForest}
#'
#' When the median/mode method is used: character vectors and factors are imputed with the mode. Numeric and integer vectors are imputed with the median.
#' When the random forest method is used predictors are first imputed with the median/mode and each variable is then predicted and imputed with that value.
#' For predictive contexts there is a \code{compute} and an \code{impute} function. The former is used on a training set to learn the values (or random forest models) to impute (used to predict).
#' The latter is used on both the training and new data to impute the values (or deploy the models) learned by the \code{compute} function.
#'
#' @param data A data frame with dummies or numeric variables. When method=="median/mode" columns can be of type "character". When method="randomForest" columns cannot be of type "character".
#' @param object If NULL \code{impute} will call \code{compute} on the current dataset. Otherwise it will accept the output of a call to \code{compute}
#' @param method Either "median/mode" or "randomForest". Only works if object = NULL
#' @param flag Add dummy variables to indicate which rows have been imputed for each variable
#' @return An imputed data frame.
#' @examples
#'#Compute the values on a training dataset and impute them on new data.
#'#This is very convenient in predictive contexts. For example:
#'
#'#define training data
#'(train <- data.frame(v_int=as.integer(c(3,3,2,5,1,2,4,6)),
#'                  v_num=as.numeric(c(4.1,NA,12.2,11,3.4,1.6,3.3,5.5)),
#'                  v_fact=as.factor(c('one','two',NA,'two','two','one','two','two')),
#'                  stringsAsFactors = FALSE))
#'
#'#Compute values on train data
#'#randomForest method
#'values <- compute(train, method="randomForest")
#'#median/mode method
#'values2 <- compute(train)
#'
#'#define new data
#'(newdata <- data.frame(v_int=as.integer(c(1,1,2,NA)),
#'                  v_num=as.numeric(c(1.1,NA,2.2,NA)),
#'                  v_fact=as.factor(c('one','one','one',NA)),
#'                  stringsAsFactors = FALSE))
#'
#'#locate the NA's
#'is.na(newdata)
#'#how many missings per variable?
#'colSums(is.na(newdata))
#'
#'#Impute on newdata
#'impute(newdata,object=values) #using randomForest values
#'impute(newdata,object=values2) #using median/mode values
#'
#'#One can also impute directly in newdata without the compute step
#'impute(newdata)
#'
#'#Flag parameter
#'impute(newdata,flag=TRUE)
#' @seealso \code{\link{compute}}
#' @author Matthijs Meire, Michel Ballings, Dirk Van den Poel, Maintainer: \email{Matthijs.Meire@@UGent.be}

impute <- function(data,  object=NULL, method="median/mode", flag=FALSE){
  if (is.null(object)) object <- compute(data, method=method)
  if (!identical(colnames(data),names(object))) stop('Variable names and variable positions need to be identical in compute and impute')

  if (flag){
      varswithmissings <- colSums(is.na(data)) > 0
      varswithmissings <- names(varswithmissings[varswithmissings])
      dums <- data.frame(sapply(data[,varswithmissings], function(x) as.factor(ifelse(is.na(x),1,0))))
      colnames(dums) <- paste0(colnames(dums),"_flag")
  }

  if (!class(object[[1]][[1]])=="randomForest"){
  data <- data.frame(sapply(1:ncol(data), function(i) {
        fact <- is.factor(data[,i])
        if (fact) data[,i] <- as.character(data[,i])
        data[is.na(data[,i]),i] <- object[[i]]
        if (fact) data[,i] <- as.factor(data[,i])
        return(data[,i,drop=FALSE])
    }, simplify = FALSE))
  } else {
    # first impute predictors only with median/mode
    predictorsImputed <- impute(data, method="median/mode")
    # then use that data to predict response
    for (i in which(sapply(data,is.factor))) levels(data[,i]) <- object[[i]][[2]]
    for (i in which(sapply(predictorsImputed,is.factor))) levels(predictorsImputed[,i]) <- object[[i]][[2]]
    for (i in 1:ncol(data)){
      predicted <- predict(object[[i]][[1]],newdata=predictorsImputed[,-i], type="response")
      # fact <- is.factor(data[,i])
      # if (fact) data[,i] <- as.character(data[,i])
      NAs <- is.na(data[,i])
      data[NAs,i] <- predicted[NAs]
      # if (fact) data[,i] <- as.factor(data[,i])

    }

  }
  if (flag){
    cbind(data,dums)
  } else {
    data
  }
}




