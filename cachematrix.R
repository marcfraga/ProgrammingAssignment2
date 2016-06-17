# makeCacheMatrix function ==> creates a function list to be
# called from within other functions.
# It saves the input to a variable 'mat' that
# Please refer to file "testing_functions.git" in this repo
# for examples and testing variables

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL         
    set <- function(y){
        x<<-y
        mat<<-NULL
    }
    get<-function() x
    setmatrix <- function(solve) mat <<- solve
    getmatrix <- function() mat
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

###########################

# cacheSolve function ==> calculates the inverse of the matrix
# stored in the above function.
# If a cached result is found, it will read the cache instead 
# of recalculating.

cacheSolve <- function(x, ...) {
    mat <- x$getmatrix()
    if(!is.null(mat)){
        message("Getting cached data")
        return(mat)
    }
    matrix <- x$get()
    mat <-solve(matrix, ...)
    x$setmatrix(mat)
    mat
}
