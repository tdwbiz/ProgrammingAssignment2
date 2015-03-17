##Functions to Manupliate Matrix
##1. makeCacheMatrix - take matrix as input, and cache it's inverse.
##2. cacheSolve - take matric as input and return inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        #init local matrix localMatrixX to NULL 
        localMatrixX <- NULL
        
        #create setter function to set passed parameter matix y to cacheX, and set cacheM to NULL in cache		
        setfunction <- function(y) {
                cacheX <<- y
                cacheM <<- NULL       
        }
        
        #create getter function to get the matrix passed in the command line call to '$set
        getfunction <- function() cacheX 
        
        #create setter function to set the value of cacheM in cache to the value of local_m localMatrixX in the call to '$set_cacheM
        set_cacheM <- function(localMatrixX) cacheM <<- localMatrixX
        
        #create getter function to get value of cacheM from cache and return cacheM to the caller so we can check it for NULL
        get_cacheM <- function() cacheM                       
        list(set = setfunction, get = getfunction,
             set_cacheM = set_cacheM,
             get_cacheM = get_cacheM)
}

## This function computes the inverse of a matrix. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
        #get value of localMatrixY from cache
        localMatrixY<- x$get_cacheM()
        #return cached matrix if available
        if(!is.null(localMatrixY)) {
                message("getting cached data")
                return(localMatrixY)
        }  
        #if not returned yet, then matrix is not in cache
        
        #get original matrix
        startM <- x$get()             
        
        #invert original matrix
        endM <- solve(startM)
        
        #set inverted matrix in cache
        x$set_cacheM(endM)
        
        #return inverted matrix
        endM 
}