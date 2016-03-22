Dendrogram3D <-function(x)
{
  row=dim(x)[1]
  col=dim(x)[2]
  a=as.vector(x);
  data=x;
  row_order=rep(-1,row)
  col_order=rep(-1,col)
  height=rep(0,row+col-2)
  row_col=rep(-1,row+col-2)
  merge= rep(0,2*(row+col-2))
  
  out <- .C ("Agl", PACKAGE="hbiclust", row_col=as.integer(as.numeric(row_col)), merge=as.integer(as.numeric(merge)), height=as.vector(as.numeric(height)), dim = as.integer(dim(data)), row_order=as.integer(as.numeric(row_order)), col_order=as.integer(as.numeric(col_order)) , data = a)
  
  b <- list("row_col" = as.numeric(out$row_col),
            "merge" = matrix(as.numeric(out$merge), nrow=(row+col-2),ncol=2,byrow=FALSE),
            "height" = out$height,
            "dim" = out$dim,
            "row_order" = out$row_order,
            "col_order" = out$col_order,
            "row_name" = rownames(data),
            "col_name" = colnames(data),
            "data" = data);
  
  class(b) <- "hbiclust";
  
  return(b)
}