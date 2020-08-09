## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix, get the value of the matrix ,set the value of the inverse, get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inver<-NULL
        set<-function(y){
                x<<-y
                inver<<-NULL                
        }
        get<-function() x
        setInverse<-function(inverse) inver <<-inverse
        getInverse<-function() inver
        list(set=set,get=get, setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by above function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver<-x$getInverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        matri<-x$get()
        inver<-solve(matri,...)
        x$setInverse(inver)
        inver
}
