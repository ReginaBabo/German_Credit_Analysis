
#' @title Generate Indexes for Cross Validation Folds from Data Set
#'
#' @description
#' Utilizes caret::createFolds to generate a list of the row indexes for k
#' unique subsets of the data. For each unique subset of indexes, combines all
#' the other subsets of indexes into single training dataset of length k-1/k.
#'
#' For instance, if dataset is of length 1000, then each element of output list
#' will be a list of indexes of length k-1/k * 1000.
#'
#' @param y a vector of values
#' @param k a positive integer of length 1, giving the number of positions to
#'   lead or lag by
#' @importFrom caret
#' @return generates a list of the row indexes for k unique subsets of the data.
#' For each unique subset of indexes, combines all the other subsets of indexes
#' into a single training dataset of length k-1/k to serve as a training set.
#' @examples
#' myFolds <- createCVFolds(credit$creditability, k = 10)
#'
#' # Create reusable trainControl object: myControl
#'     myControl <- trainControl(summaryFunction = p35,
#'                          classProbs = TRUE, # IMPORTANT!
#'                           verboseIter = FALSE,
#'                           savePredictions = TRUE,
#'                           index = myFolds
#'         )
#'
#'     train_fit = train(creditability ~ .,
#'                 method = 'glm',
#'                 family="binomial",
#'                 data = credit,
#'                 trControl= myControl)
#'
#' right <- mutate(scrambled, prev = lag(value, order_by = year))
#' arrange(right, year)
#' @name createCVFolds
NULL
#' @export


createCVFolds = function(y, k = 10) {
    eq_folds = createFolds(y, k = k)
    out = list()
    for (idx in 1:k) {
        folds = 1:k
        folds = folds[-idx]
        out[[idx]] = unlist(eq_folds[folds])
        names(out[[idx]]) = NULL
    }
    names(out) = names(eq_folds)
    return(out)
}


