
#' @title Repeated K-Fold Cross Validation Tuner
#'
#' @description
#' Wrapper for caret::train that allows training of non-tunable parameters.
#' Validates models using repeated k-fold cross validation
#'
#' @param formula formula of form y ~ x passed to caret::train
#' @param data data frame from which variables specified in formula are
#' preferentially to be taken
#' @param method a string specifying which classification or regression model
#' to use for caret::train
#' @param cutoff What probability to use to predict positive class (first class
#'  in factor levels for y)
#' @param k number of folds for k-fold cross validation
#' @param reps number of times to repeat k-fold validation
#' @param verboseIter whether iterations of the cross validation are printed
#' to the console
#' @param seed random seed to set for reproducible results
#' @param summaryFunction a function to compute performance metrics across
#' resamples passed to caret::trainControl.
#' @param ... arguments passed to caret::train and the classification or
#' regression routine (such as randomForest) .
#' @importFrom caret dplyr createCVFolds.R
#' @return a results dataframe for the accuracy metrics passed to
#' summaryFunction for the repeated k-fold cross validation
#' @examples
#' train_cv(creditability ~ .,
#'          data = credit,
#'          method = "rf",
#'          tuneGrid = tuneGridRF,
#'          cutoff = 0.5)
#' @name train_cv
NULL
#' @export

train_cv = function(formula, data, method, cutoff = 0.5, k = 10, reps = 10, verboseIter = TRUE, seed = 123, summaryFunction = p35, ...) {
    results = data.frame()
    set.seed(seed)
    for (idx in 1:reps) {
        ## Create unique set of folds
        y = data[,all.vars(as.formula(formula))[1]][[1]]
        cv_folds = createCVFolds(y, k = k)

        for (fold_idx in cv_folds) {

            train = data[fold_idx,]
            test = data[-fold_idx,]

            ## Fit Model to Fold and produce results
            myControl <- trainControl(summaryFunction = summaryFunction,
                                      classProbs = TRUE, # IMPORTANT!
                                      verboseIter = verboseIter,
                                      savePredictions = TRUE,
                                      method = "none"
            )
            ### Model Fit ###
            mod_fit = train(formula,
                            method = method,
                            data = train,
                            trControl= myControl,
                            metric = "P35",
                            ...)
            ##################

            pos_class_idx = 1
            if (method == "rlda") {
                pos_class_idx = 2
            }
            obs = test[,all.vars(as.formula(formula))[1]][[1]]
            pred = predict(mod_fit, test, type = "prob")[,pos_class_idx]
            pred = ifelse(pred > cutoff, "Good", "Poor") %>% factor(levels = levels(obs))

            out = data.frame(obs = test[,all.vars(as.formula(formula))[1]][[1]],
                             pred = factor(pred, levels=levels(test[,all.vars(as.formula(formula))[1]][[1]])))

            new = p35(out)
            results = rbind(results, new)
        }
    }
    names(results) = c("Accuracy","Specificity","P35")
    return(results)
}


#' @title Cutoff Seeker - Values 0 to 1
#'
#' @description
#' Given three parameter values runs train_cv for each value to determine
#' accuracy for repeated k-fold cross validation
#'
#' @param formula formula of form y ~ x passed to caret::train
#' @param data data frame from which variables specified in formula are
#' preferentially to be taken
#' @param method a string specifying which classification or regression model
#' to use for caret::train
#' @param low lowest of the parameter value options
#' @param mid paramater value greater than low, but smaller than high
#' @param high greatest of the parameter value options
#' @param k number of folds for k-fold cross validation
#' @param reps number of times to repeat k-fold validation
#' @param verboseIter whether iterations of the cross validation are printed
#' to the console
#' @param seed random seed to set for reproducible results
#' @param ... arguments passed to train_cv, caret::train, or the classification or
#' regression routine (such as randomForest) .
#' @importFrom caret dplyr createCVFolds.R
#' @return a list of results dataframe for the accuracy metrics from train_cv
#' for each paramater value
#' @examples
#'cutoff_seq(formula = creditability ~ .,
#'           data = credit,
#'           method = "rf",
#'           tuneGrid = tuneGridRF)
#' @name cutoff_seq
NULL
#' @export

cutoff_seq = function(formula, data, method, low = 0.25, mid = 0.5, high = 0.75, k = 10, reps = 10, verboseIter = TRUE, seed = 123, ...) {
    cutoff_options = list()
    cutoff_vec = c(low, mid, high)
    for (cutoff in cutoff_vec) {
        cutoff_options[[as.character(cutoff)]] = train_cv(formula = formula,
                                                          data = data,
                                                          method = method,
                                                          cutoff = cutoff,
                                                          k = k,
                                                          reps = reps,
                                                          verboseIter = verboseIter,
                                                          seed = seed,
                                                          ...)
    }
    return(cutoff_options)
}


#' @title Parameter Refiner - Values 0 to 1
#'
#' @description
#' Given difference between parameters, and parameter value with best performance,
#' generates a new set of paramters to drive with smaller differences in values
#'
#' @param by difference between prior parameter settings
#' @param index_max index of best parameter from prior performance check -
#' 1 = low, 2 = mid, 3 = high
#' @param key_max best parameter setting as a character vector (name)
#' @importFrom caret dplyr createCVFolds.R
#' @return a list of low, mid, and high paramter values for further performance testing
#' @examples
#' get_next_cutoffs(by = 0.25, index_max = 2, key_max = "0.5")
#' @name get_next_cutoffs
NULL
#' @export


get_next_cutoffs = function(by, index_max, key_max) {
    new_by = by - 0.10
    if (index_max == 1) {
        ##LEFT
        new_high = as.numeric(key_max)
        new_mid = new_high - new_by
        if (new_mid - new_by > 0) {
            new_low = new_mid - new_by
        }
        else {
            new_low = 0.01
        }
        return(list(low = new_low, mid = new_mid, high = new_high, by = new_by))
    }
    else if (index_max == 2) {
        ##CENTER
        new_mid = as.numeric(key_max)
        if (as.numeric(key_max) + new_by < 1) {
            new_high = as.numeric(key_max) + new_by
        }
        else {
            new_high = 0.99
        }
        if (as.numeric(key_max) - new_by > 0) {
            new_low = as.numeric(key_max) - new_by
        }
        else {
            new_low = 0.05
        }

        return(list(low = new_low, mid = new_mid, high = new_high, by = new_by))
    }
    else {
        ##RIGHT
        new_low = as.numeric(key_max)
        new_mid = new_low + new_by
        if (new_mid + new_by < 1) {
            new_high = new_mid + new_by
        }
        else {
            new_high = 0.95
        }

        return(list(low = new_low, mid = new_mid, high = new_high, by = new_by))

    }
}

#' @title Find the best parameter setting through search with cross validation
#'
#' @description
#' Given three parameter values runs train_cv for each value to determine
#' accuracy for repeated k-fold cross validation
#'
#' @param formula formula of form y ~ x passed to caret::train
#' @param data data frame from which variables specified in formula are
#' preferentially to be taken
#' @param method a string specifying which classification or regression model
#' to use for caret::train
#' @param low lowest of the parameter value options
#' @param mid paramater value greater than low, but smaller than high
#' @param high greatest of the parameter value options
#' @param by difference between low, mid, and high parameter options
#' @param k number of folds for k-fold cross validation
#' @param reps number of times to repeat k-fold validation
#' @param seed random seed to set for reproducible results
#' @param status_updates whether iterations of the parameter search progress are
#' printed to the console
#' @param target_metric_name name of the metric using to select best parameter
#' @param ... arguments passed to train_cv, caret::train, or the classification or
#' regression routine (such as randomForest) .
#' @importFrom caret dplyr createCVFolds.R
#' @return returns single row dataframe with best parameter setting and metric
#' @name get_next_cutoffs
NULL
#' @export

get_best_cutoffs = function(formula, data, method, low = 0.25, mid = 0.5, high = 0.75, by = 0.25, k = 10, reps = 10, seed = 123, status_updates = FALSE, target_metric_name = "P35", ...) {
    while(by > 0) {
        medians = lapply(cutoff_seq(low = low, mid = mid, high = high, formula = formula, data = data, method = method, k = k, reps = reps, seed = seed,...), function(x) { median(x[[target_metric_name]]) })
        if (by == 0.25 & status_updates) {
            print(list(init_cutoff = as.numeric(names(medians)[2]), init_metric = medians[[2]]))
        }
        index_max = which.max(medians)
        key_max = names(medians)[index_max]
        value_max = medians[[index_max]]

        new_cutoffs = get_next_cutoffs(by = by, index_max = index_max, key_max = key_max)
        low = new_cutoffs$low
        mid = new_cutoffs$mid
        high = new_cutoffs$high
        by = new_cutoffs$by
        if (status_updates) {
            print(list(round_cutoff = as.numeric(key_max), round_metric = value_max))
        }
    }
    return(data.frame(method = method, best_cutoff = mid, best_metric_median = value_max))
}
