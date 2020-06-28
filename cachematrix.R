
## Esta función calcula el inverso de la "matriz" especial 
##devuelta por makeCacheMatrix arriba. Si el inverso ya se ha 
##calculado (y la matriz no ha cambiado),
##entonces el caché debe recuperar el inverso del caché. 
##Calcular el inverso de una matriz cuadrada se puede hacer 
##con la función resolver en R. Por ejemplo, si X es una matriz invertible cuadrada, resolver (X) devuelve su inverso.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
