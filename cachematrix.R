## Put comments here that give an overall description of what your
## functions do

## I will write a function called "MakeCacheMatrix" to inverse the matrix and store it, 
## and another function to identify whether a matrix has been inversed.

## Write a short comment describing this function
## matr is the null matrix
## set.matr(): set the value of the vector
## get.matr(): get the value of the vector
## set.inverse(): set the value of the mean
## get.inverse(): get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
                  matr <- NULL
                    set.matr <- function(y){
                      x <<- y
                      matr <<- NULL
                    }
                    get.matr <- function() x
                    set.inverse <- function(solve) matr <<- solve
                    get.inverse <- function() matr
                    list(set.matr = set.matr, get.matr = get.matr,
                         set.inverse = set.inverse,
                         get.inverse = get.inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        matr <- x$get.inverse()
        if(!is.null(matr)){
          message("getting inversed matrix")
          return(matr)
        }
        data <- x$get.matr()
        matr <- solve(data, ...)
        x$set.inverse(matr)
        matr
        ## Return a matrix that is the inverse of 'x'
}
