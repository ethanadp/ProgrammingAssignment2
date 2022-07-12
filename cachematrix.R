## These functions are practically the same as the given examples for using the
## cache of a vector, but instead it stores the cache of a randomized matrix
## or one that is set by the user. 

## This function will set random values from 1 to 1000 for the values in a 4x4
## matrix while scoping out the solution within the cacheSolve function.
makeCacheMatrix <- function(x = matrix(sample(1:1000,16),4,4)) {
    s <- NULL                ## Will set any previously solved matrix back to 0.
   
    set <- function(y) {     ## This allows for the setting of a matrix without
        x <<- y              ## running the entire function again using the $
        s <<- NULL           ## operator.
    }
    get <- function() x      ## Will return the matrix that was randomly sampled.
    setsolution <- function(solve) s <<- solve ## Sets the solution to the matrix.
    getsolution <- function() s ## Will return the solution (inverse) of the matrix.
    list(set = set, get = get,  ## Essentially caches the values for all the functions
         setsolution = setsolution,  ## defined within this environment to be used
         getsolution = getsolution)  ## in the following function.
}

## This function is an extension of the last, where the last function will use
## lexical scoping to pull the set solution within this environment so if the 
## user decides to use '$getSolution()', the program will return the inverse
## matrix AS LONG as the set solution is not 'NULL'.
cacheSolve <- function(x, ...) {
    s <- x$getsolution() ## Recalls the solution if the solution was set already.
    if(!is.null(s)) {    ## Prints this message along with the inverse matrix.
        message("Getting the cached inverse of set matrix:")
        return(s)
    }
    data <- x$get()      ## One of the most important parts, defining the matrix
                         ## that was set to a variable so that it's solved.
    s <- solve(data, ...)  ## Setting the solution of the matrix to a variable.
    x$setsolution(s)     ## Setting the solution so that it's used in x$setSolution()
    s                    ## Prints the inverse of the matrix once solved.
}
