#' Calculate euclidean norm of a numerical vector.
#' 
#' @param x vector of numeric values.
#' @return the square root of the sum of squared values in \code{x}.
#' @examples
#' euclidNorm(c(1, 3, 0, 4))
#' euclidNorm(c(-1, 0, 5))
#' @export
euclidNorm <- function(x) {sqrt(sum(x ^ 2))}

#' Determine sign of distance from linear separator
#' 
#' @param data matrix or data frame of numeric values.
#' @param weight vector of numeric values.
#' @return -1 if distance is negative else 1
#' @examples
#' vals <- data.frame(4, 3, -2, 9)
#' weights <- c(1, 1, 1, 1)
#' signum(vals, weights)
#' @export
signum <- function(data, weight) {
    distances <- vector(length = nrow(data))
    distances <- apply(data, 1, distanceFromSeparator, weight)
    ifelse(distances < 0, -1, 1)
}

#' Calculate signed distance of point from linear separator
#' 
#' @param data matrix or vector of numeric values
#' @param weight vector of numeric values
#' @return If both arguments are vectors of same length, it will return the inner product. If one argument is a vector, it will be promoted to either a row or column matrix.
#' @examples
#' vec1 <- c(1, 2, 3)
#' vec2 <- c(4, 5, 6)
#' distanceFromSeparator(vec1, vec2)
#' @export
distanceFromSeparator <- function(data, weight) {
    distance <- data %*% weight 
    distance
}

#' Generate perceptron model.
#'
#' @description Fit a single-layer perceptron model
#' @param formula formula expression as for regression models, of the form response ~ predictors. 
#' @param data data frame in which to interpret the variables occurring in formula
#' @param learningRate integer value determining the magnitude of the weight updates (default 1)
#' @param activation function to control neuron activation (default signum)
#' @details This function implements a model for linear classification based on the perceptron model. The level of the response variable must be binary.
#' @note The learning algorithm for the perceptron model is only guaranteed to converge for linearly separable input data!
#' @return
#' \describe{
#' \item{w}{vector of best weight values found}
#' \item{coefficients}{vector of weight values normalized by euclidean distance}
#' \item{updates}{count of weight updates}
#' \item{formula}{character representation of formula}
#' \item{call}{character representation of the call command}
#' \item{x}{model matrix}
#' \item{y}{vector of response values}
#' \item{options}{list of character representation of modelling options}
#' }
#' @references Cristianini, Nello und John Shawe-Taylor (2000): \emph{An Introduction to Support Vector Machines: And Other Kernel-Based Learning Methods}, Cambridge University Press: Cambridge, England.
#' @examples
#' data(iris)
#' head(iris, n=20)
#' iris_sub <- iris[1:100, c(1, 3, 5)]
#' names(iris_sub) <- c("sepal", "petal", "species")
#' head(iris_sub)
#' formula <- formula(species ~ sepal + petal)
#' p1 <- newPerceptronModel(formula, iris_sub)
#' @import stats
#' @export
newPerceptronModel <- function(formula, data, learningRate = 1, activation = signum) {
    if(!is.data.frame(data)) stop("Input data must be of type dataframe.")

    # model matrix
    mf <- model.frame(formula, data)
    x <- model.matrix(formula, mf)
    respondName <- as.character(attr(terms(mf), "variables"))[2]
    if(nlevels(data[respondName] != 2)) stop("Invalid number of levels in response variable detected. Response variable must be binary!")

    # response vector
    y <- get(respondName, mf)
    yLab <- as.character(y)
    y <- factor(y)
    y <- ifelse(y == levels(y)[1], -1, 1)
    y <- cbind(y, yLab)
    colnames(y) <- c("class", respondName)
    y <- data.frame(y, stringsAsFactors = FALSE)
    y$class <- as.numeric(y$class)
    
    w <- vector(length = ncol(x)) # coefficient vector
    c <- 0 # weight update counter
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

    # assemble output object
    perceptronOut <- list()
    class(perceptronOut) <- "perceptron"

    perceptronOut$weights <- w
    perceptronOut$respondName <- respondName
    perceptronOut$coefficients <- coefficients
    perceptronOut$updates <- c
    perceptronOut$formula <- formula
    perceptronOut$call <- match.call()
    perceptronOut$x <- x
    perceptronOut$y <- y
    perceptronOut$yMapping <- unique(y)
    perceptronOut$options <- list(learningRate, as.character(substitute(activation)))
    names(perceptronOut$options) <- c("Learning rate", "Activation function")
    return(perceptronOut)
}                


#' Print perceptron model.
#'
#' @param x an object of class \code{perceptron} as returned by perceptron. 
#' @param ... arguments passed to or from other methods.
#' @examples
#' data(iris)
#' head(iris, n=20)
#' iris_sub <- iris[1:100, c(1, 3, 5)]
#' names(iris_sub) <- c("sepal", "petal", "species")
#' head(iris_sub)
#' formula <- formula(species ~ sepal + petal)
#' p1 <- newPerceptronModel(formula, iris_sub)
#' print(p1)
#' @export
print.perceptron <- function(x, ...) {
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    if (length(coef(x))) {
        cat("Weights:\n")
        print(coef(x))
        cat("\n")
        cat("Epochs:\n")
        cat(x$updates,"\n")
    } else {
        cat("No coefficients\n")
        cat("\n")
        invisible(x)
    }
}

#' Assemble summary output of fitted perceptron model.
#'
#' @param object an object of class \code{perceptron} as returned by perceptron.
#' @param ... arguments passed to or from other methods
#' @return
#' \describe{
#' \item{coefficients}{vector of weight values normalized by euclidean distance}
#' \item{options}{list of character representation of modelling options}
#' \item{input}{number of input layer nodes}
#' \item{hidden}{number of hidden layer nodes (fixed to 0)}
#' \item{output}{number of output layer nodes (fixed to 1)}
#' }
#' @examples
#' data(iris)
#' head(iris, n=20)
#' iris_sub <- iris[1:100, c(1, 3, 5)]
#' names(iris_sub) <- c("sepal", "petal", "species")
#' head(iris_sub)
#' formula <- formula(species ~ sepal + petal)
#' p1 <- newPerceptronModel(formula, iris_sub)
#' summary(p1)
#' @export
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


#' Print summary output of a fitted perceptron model.
#'
#' @param x an object of class \code{perceptron} as returned by perceptron. 
#' @param ... arguments passed to or from other methods
#' @examples
#' data(iris)
#' head(iris, n=20)
#' iris_sub <- iris[1:100, c(1, 3, 5)]
#' names(iris_sub) <- c("sepal", "petal", "species")
#' head(iris_sub)
#' formula <- formula(species ~ sepal + petal)
#' p1 <- newPerceptronModel(formula, iris_sub)
#' summary(p1)
#' @export
print.summary.perceptron <- function(x, ...) {
    networkDescription <- paste(x$input-1, x$hidden, x$output, sep = "-")
    cat("\nResult:\n")
    cat("A", networkDescription, "network with", x$input, "weights\n", sep = " ")
    cat("\n")
    print(coef(x))
    cat("\n\n")    
    optList <- vector(length = length(x$options))
    for (i in 1:length(x$options)) {
        optString <- paste(attr(x$options[i], "names"),
                           ": ",
                           as.character(x$options[i]),
                           sep = "")
        optList[i] <- optString
    }
    cat(optList, sep = ",  ")
    cat("\n")
}    

#' Plot of a fitted perceptron model.
#'
#' @param x an object of class \code{perceptron} as returned by perceptron. 
#' @param ... arguments passed to or from other methods
#' @examples
#' data(iris)
#' head(iris, n=20)
#' iris_sub <- iris[1:100, c(1, 3, 5)]
#' names(iris_sub) <- c("sepal", "petal", "species")
#' head(iris_sub)
#' formula <- formula(species ~ sepal + petal)
#' p1 <- newPerceptronModel(formula, iris_sub)
#' plot(p1, title = "Perceptron Classifier")
#' @import ggplot2 
#' @export
plot.perceptron <- function(x, ...) {
    if(ncol(x$x) != 3) stop("Plot functionality is only available for 2d input data")
    y <- x$y
    w <- x$weights
    intercept <- -w[1] / w[3]
    slope <- -w[2] / w[3]
    df <- data.frame(x$x, stringsAsFactors = FALSE)

    ggplot(df, aes(x = df[2], y = df[3])) +
        geom_point(aes(color = y[,2]), size = 3) +
        scale_color_discrete(name = colnames(y)[2]) +
        xlab(attr(df[2], "names")) +
        ylab(attr(df[3], "names")) +
        geom_abline(aes(intercept = intercept, slope = slope), col = "green")
}
    
#' Predict function for a fitted perceptron model
#'
#' @param object fitted \code{perceptron} model.
#' @param newdata data frame from values for which to predict the class
#' @param ... arguments passed to or from other methods
#' @examples
#' @export
predict.perceptron <- function(object, newdata, ...) {
    perceptronPredOut <- list()
#    class(perceptronPredOut) <- "prediction.perceptron"
    yMapping <- object$yMapping
    w <- object$weights
    input <- cbind(1, newdata)
    rownames(input) <- c()
    colnames(input)[1] <- "bias"
    predictions <- signum(input, w)
    predictions <- as.factor(ifelse(predictions == yMapping[1,1], yMapping[1,2], yMapping[2,2]))
    outputFrame <- data.frame(input[,-1], prediction = predictions)
    outputFrame
}

