## Function to cache the matrix input x along with its inverse. This 
## function exposes four utility functions to get/set values of the, 
## matrix and getinverse/setinverse to retrive/set the matrix inverse. 
makeCacheMatrix <- function(x = matrix()) {
        matrix_inv <- NULL
        set <- function(matrix_input) {
                x <<- matrix_input
                matrix_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) matrix_inv <<- inv
        getinverse <- function() matrix_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to compute the inverse of the matrix input x. The inverse
## output is retrieved directly if it was calculated and cached
## previously for x, and if not it is calculated and cached for  
## the future use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("\nFound cached matrix inverse: ")
                print(inv)
                return(inv)
        }
        message("\nNo results found in cache, computing and saving matrix inverse for future use: ")
        data <- x$get()
        inv <- solve(data)
        print(inv)
        x$setinverse(inv)
        return(inv)
}

## Sample testcase program :
## matrix_input <- makeCacheMatrix(matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol=3, nrow=3))
## message("Input Matrix: \n")
## print(matrix_input$get())
## result_1 <- cacheSolve(matrix_input)
## result_2 <- cacheSolve(matrix_input)

## Console output:
## Input Matrix: 
##
##    [,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1

##No results found in cache, computing and saving matrix inverse for future use: 
##     [,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1

##Found cached matrix inverse: 
##     [,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1
