cutforest <- function(b, K)
{
  rdend<-list( "merge"=b$merge[b$row_col==0,],
               "height"=b$height[b$row_col==0],
               "order"= b$row_order,
               "labels"=NULL,
               "method"="complete",    
               "dist.method"="euclidean");
  class(rdend)<-"hclust";
  #cutree(rdend, k=3)
  #rdend<- as.dendrogram(rdend)
  #b$row_order<- order.dendrogram(rdend)
  
  cdend<-list( "merge"=b$merge[b$row_col==1,],
               "height"=b$height[b$row_col==1],
               "order"= b$col_order,
               "labels"=NULL,
               "method"="complete",
               "dist.method"="euclidean");
  class(cdend)<-"hclust";
  # cdend<- as.dendrogram(cdend)
  # b$col_order<- order.dendrogram(cdend)
  rind=cutree(rdend, k=K)
  cind=cutree(cdend, k=K)
  groups <- list("row-group"=cbind(b$row_name, rind), "col-group"=cbind(b$col_name, cind) )
  class(groups) <- "bigroup";
  return(groups)
}



