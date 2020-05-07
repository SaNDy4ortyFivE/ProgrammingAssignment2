## Put comments here that give an overall description of what your
## functions do
##makecachematrix() stores a matrix and its inverse
##It can be updated via cacheSolve()

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## current inverse of matrix is assigned to null
        matrix_inverse <- NULL
        setMatrix <- function(matrixY){
                ##updating the matrix and setting it inverse to null
                x <<- matrixY
                matrix_inverse <<- NULL
        }
        ##getMatrix returns the matrix i.e value of x
        getMatrix <- function(){x}
        ##setInverse assigns parameter(inverse of matrix x) to matrix x  
        setInverse <- function(matInv){
                matrix_inverse <<- matInv	
        }
        ##returns inverse of matrix x
        getInverse <- function(){
                matrix_inverse	
        }
        list(setMatrix = setMatrix,getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
##cacheSolve checks if inverse of matrix is present in cache or not
## if present it returns the cache value
##else it computes inverse for the matrix and updates the same
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##getting inverse of matrix
        matInv <- x$getInverse()
        if(!is.null(matInv)){
                #if inverse of this matrix exists in cache return this cached value
                return(matInv)
        }
        ##get this matrix
        newMat <- x$getMatrix()
        ##solve its inverse
        matInv <- solve(newMat)
        ##update the cache with new inverse 
        x$setInverse(matInv)
        ##print inverse
        matInv
}

