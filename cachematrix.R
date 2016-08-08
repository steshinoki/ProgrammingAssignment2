## Getting the inverse of a matrix

## this will store the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                inv<<- NULL
                x<<-y
        }
        get<-function()x
        setinv<- function(inverse)inv<<- inverse
        getinv<-function()inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## this one will compute the inverse of the matrix created by makeCacheMatrix.
##it will give the inverse of the cache if the inverse of the matrrix created by makeCacheMatrix has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv()
        if(!is.null(inv)) {
                message("running data")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setinv(inv)
        inv
}

}
