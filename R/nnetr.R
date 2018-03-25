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
    respondName <- as.character(attr(terms(mf), "variables"))[2]
    if(nlevels(data[respondName] != 2)) stop("levels detected. Response variable must be binary!")
    
    y <- get(respondName, mf)
    yLab <- as.character(y)
    y <- factor(y)
    y <- ifelse(y == levels(y)[1], -1, 1)
    y <- cbind(y, yLab)
    colnames(y) <- c("class", respondName)
    y <- data.frame(y, stringsAsFactors = FALSE)
    y$class <- as.numeric(y$class)
 
    w <- vector(length = ncol(x))
    c <- 0
    weightUpdate <- TRUE    
    R <- max(apply(x, 1, euclidNorm))
    while (weightUpdate) {
        weightUpdate <- FALSE
        yClassified <- activation(x, w)
        
        for (i in 1:nrow(x)) {
            if (y[i,1] != yClassified[i]) {
                w[-1] <- w[-1] + learningRate * y[i,1] * x[i,-1]
                w[1] <- w[1] + learningRate * y[i,1] * R^2
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

    perceptronOut$w <- w
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


plot.perceptron <- function(object, title, ...) {
    if(ncol(object$x) != 3) stop("Plot functionality is only available for 2d input data")
    x <- data.frame(object$x)
    y <- object$y
    weights <- object$w
    intercept <- -weights[1] / weights[3]
    slope <- -weights[2] / weights[3]
    
    ggplot(x, aes(x = x[2], y = x[3])) +
        geom_point(aes(color = y[,2]), size = 3) +
        scale_color_discrete(name = colnames(y)[2]) +
        xlab(attr(x[2], "names")) +
        ylab(attr(x[3], "names")) +
        ggtitle(title) +
        geom_abline(aes(intercept = intercept, slope = slope), col = "green")

}
    
