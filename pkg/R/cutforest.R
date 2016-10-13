cutforest <- function(b, k = NULL, h = NULL)
{
  if (is.null(k) && is.null(h)) 
    stop("either 'k' or 'h' must be specified")
  
  rdend<-list( "merge"=b$merge[b$row_col==0,],
               "height"=b$height[b$row_col==0],
               "order"= b$row_order,
               "method"="ward",    
               "dist.method"="euclidean");
  class(rdend)<-"hclust";
  #cutree(rdend, k=3)
  #rdend<- as.dendrogram(rdend)
  #b$row_order<- order.dendrogram(rdend)
  
  cdend<-list( "merge"=b$merge[b$row_col==1,],
               "height"=b$height[b$row_col==1],
               "order"= b$col_order,
               "method"="ward",
               "dist.method"="euclidean");
  class(cdend)<-"hclust";
  # cdend<- as.dendrogram(cdend)
  # b$col_order<- order.dendrogram(cdend)
  
  if ( is.null(h) )
  {
    i=1
    trow=b$dim[1];
    tcol=b$dim[2];
    while((trow*tcol)>k)
    {        
      if((b$row_col[i]==0) && (trow>1))
        trow=trow-1;
      if((b$row_col[i]==1) && (tcol>1))
        tcol=tcol-1;
      i=i+1
    }
    
    rind=cutree(rdend, k=trow)
    cind=cutree(cdend, k=tcol)
  }
  else
  {
    rind=cutree(rdend, h=h)
    cind=cutree(cdend, h=h)
  }
  
  if(is.null(b$row_name))
    row.group <- rind
  else
    row.group <- data.frame(b$row_name, rind)
  
  if(is.null(b$col_name))
    col.group <- cind
  else
    col.group <- data.frame(b$col_name, cind)
  
  CCE=ColClusterEffect(b, rind, cind)
    
  groups <- list(row.group, col.group, CCE)
  class(groups) <- "bigroup";
  return(groups)
}