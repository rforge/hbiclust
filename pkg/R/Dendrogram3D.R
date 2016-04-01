hbiclust <-function(x, method="ward")
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
  m=3
  if(method=="average"){m=1}
  if(method=="centroid"){m=2}
  if(method=="ward"){m=3}
  if(method=="single"){m=4}
  if(method=="complete"){m=5}
  
  out <- .C ("Agl", PACKAGE="hbiclust", row_col=as.integer(as.numeric(row_col)), method=as.integer(as.numeric(m)), merge=as.integer(as.numeric(merge)), height=as.vector(as.numeric(height)), dim = as.integer(dim(data)), row_order=as.integer(as.numeric(row_order)), col_order=as.integer(as.numeric(col_order)) , data = a)
  
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
  
  rdend<-list( "merge"=b$merge[b$row_col==0,],
               "height"=b$height[b$row_col==0],
               "order"= b$row_order,
               "labels"=NULL,
               "method"=method,    
               "dist.method"="euclidean");
  class(rdend)<-"hclust";
  
  cdend<-list( "merge"=b$merge[b$row_col==1,],
               "height"=b$height[b$row_col==1],
               "order"= b$col_order,
               "labels"=NULL,
               "method"=method,
               "dist.method"="euclidean");
  class(cdend)<-"hclust";
  ch=BIC(data, rdend, cdend, b$row_col, row, col, b$height)
  remove(b)
  hbi <- list("row_col" = as.numeric(out$row_col),
            "merge" = matrix(as.numeric(out$merge), nrow=(row+col-2),ncol=2,byrow=FALSE),
            "height" = out$height,
            "dim" = out$dim,
            "row_order" = out$row_order,
            "col_order" = out$col_order,
            "row_name" = rownames(data),
            "col_name" = colnames(data),
            "data" = data,
            "hcut"=ch);
  
  class(hbi) <- "hbiclust";
  return(hbi)
}
BIC <- function(data, rdend, cdend, row_col, row, col, height)
{
  minBIC=1000000000;
  i=1;
  trow=row;
  tcol=col;
  while((trow*tcol)>1)
  {        
    bc=0;
    if((row_col[i]==0) && (trow>1))
    {            
      trow=trow-1;
      rind=cutree(rdend, k=trow);
      cind=cutree(cdend, k=tcol);
      
      for(ri in 1:max(rind))
        for(ci in 1:max(cind))
        {
          tdat=data[rind==ri, cind==ci];
          if(class(tdat)=="matrix")                
          {                        
            mns = colMeans(tdat);
            bc = bc+(sum(sweep(tdat, 2, mns)^2)/nrow(tdat));                        
          }
        }
    }
    if((row_col[i]==1) && (tcol>1))
    {            
      tcol=tcol-1;
      rind=cutree(rdend, k=trow);
      cind=cutree(cdend, k=tcol);
      
      for(ri in 1:max(rind))
        for(ci in 1:max(cind))
        {
          tdat=data[rind==ri, cind==ci];
          if(class(tdat)=="matrix")                    
          {
            mns = rowMeans(tdat);
            bc = bc+(sum(sweep(tdat, 1, mns)^2)/ncol(tdat));                        
          }
        }
    }
    bc=bc+((trow*tcol)*log10(row*col));
    #print(c(i, trow, tcol))
    if(bc<minBIC && trow>1 && tcol>1)
    {
      minBIC=bc;
      ch = height[i];
    }        
    i=i+1;        
  }
  #ch= height[i-2]-3;
  return(ch);
}

