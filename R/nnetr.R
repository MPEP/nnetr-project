euclidNorm <- function(x) {sqrt(sum(x^2))}


unitStep <- function(data, weight) {
    distances <- vector(length = nrow(data))
    distances <- apply(data, 1, distanceFromSeparator, weight)
    classification <- ifelse(distances < 0, -1, 1)
    unitStepOut <- structure(classification, name = "Unit Step")
    unitStepOut
}



distanceFromSeparator <- function(data, weight) {
    distance <- data %*% weight
    distance
}

newPerceptronModel <- function(formula, data, learningRate = 1, activation = unitStep) {
    if(!is.data.frame(data)) stop("Input data must be dataframe")
    #if(!is.numeric()) stop("Input must be numeric")
   
    mf <- model.frame(formula, data)
    x <- model.matrix(formula, mf)
    y <- model.response(mf)
    w <- vector(length = ncol(x))
    c <- 0
    weightUpdate <- TRUE    
    R <- max(apply(x, 1, euclidNorm))
    
    while (weightUpdate) {
        weightUpdate <- FALSE
        yClassified <- activation(x, w)
        for (i in 1:nrow(x)) {
            if (y[i] != yClassified[i]) {
                w[-1] <- w[-1] + learningRate * y[i] * x[i,-1]
                w[1] <- w[1] + learningRate * y[i] * R^2
                c <- c + 1
                weightUpdate <- TRUE
            }
        }
    }
    
    s <- euclidNorm(w)
    coefficients <- w / s
    names(coefficients) <- c("bias", attr(terms.formula(formula),"term.labels"))

    perceptronOut <- list()
    class(perceptronOut) <- "perceptron"

    perceptronOut$coefficients <- coefficients
    perceptronOut$updates <- c
    perceptronOut$formula <- formula
    perceptronOut$call <- match.call()
    perceptronOut$x <- x
    perceptronOut$y <- y
    perceptronOut$lr <- learningRate
    perceptronOut$activation <- attr(activation, "name")
    
    return(perceptronOut)
}                

print.perceptron <- function(model, digits = 2, ...) {
    cat("\nCall:\n", deparse(model$call), "\n\n", sep = "")
    if (length(coef(model))) {
        cat("Weights:\n")
        print(coef(model))
        cat("\n")
        cat("Epochs:\n")
        cat(model$updates,"\n")
    } else {
        cat("No coefficients\n")
        cat("\n")
        invisible(model)
    }
}


summary.perceptron <- function(object, ...) {
    perceptronSumOut <- list()
    class(perceptronSumOut) <- "summary.perceptron"
    perceptronSumOut$model <- object
    perceptronSumOut$options <- list(learningRate = object$lr)
}

## plot.perceptron
## intercept <- -w[1] / w[3]
## slope <- -w[2] / w[3]


    
