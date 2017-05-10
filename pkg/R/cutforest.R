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
  
  #CIL=clusterimpBF(b, rind, cind)
  #CIB=clusterimpact(b, rind, cind)
    
  groups <- list("Row_grouping"=row.group, "Col_grouping"= col.group)
  
  #######################
  # par(mfrow=c(2,2))
  # barplot(CIL$row_likelihood[1,], col = heat.colors(length(rind)), main = "rows impact", ylab = "Log Bayes factor")
  # barplot(CIL$row_likelihood[2,], col = heat.colors(length(rind)), main = "row clusters impact", ylab = "Log Bayes factor")
  # barplot(CIB$row_likelihood[1,]/sum(CIB$row_likelihood[1,]), col = heat.colors(length(rind)), main = "rows impact", ylab = "between sum of square")
  # barplot(CIB$row_likelihood[2,]/sum(unique(CIB$row_likelihood[2,])), col = heat.colors(length(rind)), main = "row clusters impact", ylab = "between sum of square")
  # dev.new()
  # par(mfrow=c(2,2))
  # barplot(CIL$col_likelihood[1,], col = heat.colors(length(cind)), main = "cols impact", ylab = "Log Bayes factor")
  # barplot(CIL$col_likelihood[2,], col = heat.colors(length(cind)), main = "col clusters impact", ylab = "Log Bayes factor")
  # barplot(CIB$col_likelihood[1,]/sum(CIB$col_likelihood[1,]), col = heat.colors(length(cind)), main = "cols impact", ylab = "between sum of square")
  # barplot(CIB$col_likelihood[2,]/sum(unique(CIB$col_likelihood[2,])), col = heat.colors(length(cind)), main = "col clusters impact", ylab = "between sum of square")
  #######################
  #dev.new()
  # strl=sort(CI$row_likelihood[1,], method = "shell", index.return = TRUE)
  # rname = b$row_name[strl$ix]
  
  # strlnew=sort(CInew$row_likelihood[1,], method = "shell", index.return = TRUE)
  # rnamenew = b$row_name[strlnew$ix]

  # if(is.null(rname))
  #   rname = strl$ix
  # barplot(strl$x, names.arg = rname, las=2, col = heat.colors(length(strl$x)), main = "rows impact", xlab = "row names", ylab = "Log Bayes factor")

  # if(is.null(rnamenew))
  #   rname = strlnew$ix
  # barplot(strlnew$x, names.arg = rnamenew, las=2, col = heat.colors(length(strl$x)), main = "rows impact", xlab = "row names", ylab = "Log Bayes factor")
  ##################################################################################################################################
  # stcl=sort(CI$col_likelihood[1,], method = "shell", index.return = TRUE)
  # cname = b$col_name[stcl$ix]
  # if(is.null(cname))
  #   cname = stcl$ix
  # barplot(stcl$x, names.arg = cname, las=2, col = heat.colors(length(stcl$x)), main = "columns impact", xlab = "column names", ylab = "Log Bayes factor")

  class(groups) <- "bigroup";
  return(groups)
}