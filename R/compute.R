#' Compute the missing values to later impute them in another dataset
#'
#' When the median/mode method is used: character vectors and factors are imputed with the mode. Numeric and integer vectors are imputed with the median.
#' When the random forest method is used predictors are first imputed with the median/mode and each variable is then predicted and imputed with that value.
#' For predictive contexts there is a \code{compute} and an \code{impute} function. The former is used on a training set to learn the values (or random forest models) to impute (used to predict).
#' The latter is used on both the training and new data to impute the values (or deploy the models) learned by the \code{compute} function.
#'
#' @param data A data frame with dummies or numeric variables. When method=="median/mode" columns can be of type "character". When method="randomForest" columns cannot be of type "character".
#' @param method Either "median/mode" or "randomForest"
#' @param ... additional arguments for \code{randomForest}
#' @return Values or models used for imputation
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
#' @seealso \code{\link{impute}}
#' @author Matthijs Meire, Michel Ballings, Dirk Van den Poel, Maintainer: \email{Matthijs.Meire@@UGent.be}
compute <- function (data, method="median/mode", ...)
{

    if (method=="median/mode") {
        Mode <- function(x) {
            xtab <- table(x)
            xmode <- names(which(xtab == max(xtab)))
            return(xmode[1])
        }

        values <- sapply(data, function(x) {
            if (class(x) %in% c("character", "factor"))
                Mode(x)
            else if (class(x) %in% c("numeric", "integer"))
                median(x, na.rm = TRUE)

        }, simplify = FALSE)
    } else if (method=="randomForest") {

        data <- impute(data, method="median/mode")

        values <- list()
        for (i in 1:ncol(data)){
            values[[i]] <- list()
            values[[i]][[1]] <- randomForest(x=data[,-i,drop=FALSE],y= data[,i], ...)
            values[[i]][[2]] <- levels(data[,i])
        }
        names(values) <- colnames(data)
    }
    values
}



