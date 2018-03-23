euclidNorm <- function(x) {sqrt(sum(x ^ 2))}

unitStep <- function(data, weight) {
    distances <- vector(length = nrow(data))
    distances <- apply(data, 1, distanceFromSeparator, weight)
    ifelse(distances < 0, -1, 1)
}

distanceFromSeparator <- function(data, weight) {
    distance <- data %*% weight
    distance
}

newPerceptronModel <- function(formula, data, learningRate = 1, activation = unitStep) {
    if(!is.data.frame(data)) stop("Input data must be of type dataframe")
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
    perceptronOut$options <- list(learningRate, as.character(substitute(activation)))
    names(perceptronOut$options) <- c("Learning rate", "Activation function")
    return(perceptronOut)
}                

print.perceptron <- function(object, digits = 2, ...) {
    cat("\nCall:\n", deparse(object$call), "\n\n", sep = "")
    if (length(coef(object))) {
        cat("Weights:\n")
        print(coef(object))
        cat("\n")
        cat("Epochs:\n")
        cat(object$updates,"\n")
    } else {
        cat("No coefficients\n")
        cat("\n")
        invisible(object)
    }
}

summary.perceptron <- function(object, ...) {
    perceptronSumOut <- list()
    class(perceptronSumOut) <- "summary.perceptron"

    neuronLabels <- vector(length = length(object$coefficients))
    
    for (i in 1:length(object$coefficients)) {
        neuronLabel <- paste(attr(object$coefficients[i], "names"),
                           "->",
                           "o",
                           sep = "")
        neuronLabels[i] <- neuronLabel
    }

    perceptronSumOut$coefficients <- object$coefficients
    names(perceptronSumOut$coefficients) <- neuronLabels
    perceptronSumOut$options <- object$options
    perceptronSumOut$input <- ncol(object$x)
    perceptronSumOut$hidden <- 0
    perceptronSumOut$output <- 1
    return(perceptronSumOut)
}

print.summary.perceptron <- function(object, digits = 4, ...) {
    networkDescription <- paste(object$input-1, object$hidden, object$output, sep = "-")
    cat("\nResult:\n")
    cat("A", networkDescription, "network with", object$input, "weights\n", sep = " ")
    cat("\n")
    print(coef(object))
    
    ## for (i in 1:length(object$coefficients)) {
    ##     weightString <- paste(attr(object$coefficients[i], "names"),
    ##                        "->",
    ##                        "o",
    ##                        sep = "")
    ##     weightList[i] <- weightString
    ## }
    
    cat("\n\n")
        
    optList <- vector(length = length(object$options))
    for (i in 1:length(object$options)) {
        optString <- paste(attr(object$options[i], "names"),
                           ": ",
                           as.character(object$options[i]),
                           sep = "")
        optList[i] <- optString
    }
    cat(optList, sep = ",  ")
    cat("\n")
}    


plot.perceptron(object, ...) {
    w <- object
    intercept <- -w[1] / w[3]
    slope <- -w[2] / w[3]
    ggplot(df, aes(x = sepal, y = petal)) +
        geom_point(aes(colour = species), size = 3) +
        xlab("Sepal length") +
        ylab("Petal length") +
        ggtitle("Species vs Sepal and Petal lengths") +
        geom_abline(aes(intercept = attr(p1, "intercept"), slope = attr(p1, "slope")), col = "green")
    
## intercept <- -w[1] / w[3]
## slope <- -w[2] / w[3]


    
