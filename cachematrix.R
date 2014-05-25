#Programming assignment2 involves two functions:
##makeCacheMatrix: This function creates a special "matrix" object that can cache 
##                 its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##            makeCacheMatrix above. If the inverse has already been calculated 
##            (and the matrix has not changed), then the cachesolve should retrieve 
##            the inverse from the cache.

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## makeCacheMatrix function calculates the inverse of input matrix 'x' and 
## stores the input matrix 'x' and its inverse 'm' in cache

makeCacheMatrix <- function(x = matrix()) {
    
    y <<- x    
    m <<- solve(x)
    
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## cacheSolve function first checks if inverse of the input matrix is already calculated
## If yes then it populates the value stored in cache
## If not then it calculates the inverse freshly

cacheSolve <- function(x=matrix(),...) {
    
    n<-matrix(nrow=nrow(m),ncol=ncol(m))
    
    ## Assign the matrix for which inverse is already calculated to 'orig'
    orig<-y
    
    
    ## identical(x,orig) finds if matrix input to cacheSolve function 
    ##  is same as the original matrix for which inverse has been calculated
    
    ## is.matrix(m) checks if the m object found is a matrix only
    
    ## identical(m,n)==FALSE   checks that m matrix is not null by comparing it with 'n' matrix
    
    if(identical(x,orig) & is.matrix(m) & identical(m,n)==FALSE) {
        message("getting cached data")
        return(m)
    }
    else {
        ## if the input matrix to cacheSolve function is different from the 
        ## one for which inverse was calculated then calculate the inverse matrix k
        
        k <- solve(x)
        message("calculating inverse")
        return(k)}
}    

