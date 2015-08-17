".onLoad" <-function(lib, pkg)
{
  library.dynam("hbiclust", package = pkg, lib.loc = lib)
  return(invisible(0)) 
}
