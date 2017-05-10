clusterimpBF <- function(b, rind, cind)
{
  sig=empericalsig(b, rind, cind)
  bfcc = rep(0, max(cind))
  for(ci in 1:max(cind))
  {
    for(ri in 1:max(rind))
    {
      tdat1=as.vector(b$data[rind==ri, cind==ci])
      bfcc[ci] = bfcc[ci] + lognormal(tdat1, sig)
    }
    tdat=as.vector(b$data[, cind==ci])
    bfcc[ci] = bfcc[ci] - lognormal(tdat, sig)
  }
  clc=cind
  for(ci in 1:max(cind))
  {
   clc[cind==ci]=bfcc[ci]
  }
  ###########################################################
  bfrc = rep(0, max(rind))
  for(ri in 1:max(rind))
  {
    for(ci in 1:max(cind))
    {
      tdat1=as.vector(b$data[rind==ri, cind==ci])
      bfrc[ri] = bfrc[ri] + lognormal(tdat1, sig)
    }
    tdat=as.vector(b$data[rind==ri,])
    bfrc[ri] = bfrc[ri]-lognormal(tdat, sig)
  }
  rlc=rind
  for(ri in 1:max(rind))
  {
    rlc[rind==ri]=bfrc[ri]
  }
  ###################################################################
  bfr = rep(0, length(rind))
  for(ri in 1:length(rind))
  {
    tdat=as.vector(b$data[ri, ])
    for(ci in 1:max(cind))
    {
      tdat1=as.vector(b$data[ri, cind==ci])      
      bfr[ri] = bfr[ri] +lognormal(tdat1, sig)
    }
    bfr[ri] = bfr[ri]-lognormal(tdat, sig)
  }  
    
  bfc = rep(0, length(cind))
  for(ci in 1:length(cind))
  {
    tdat=as.vector(b$data[, ci])
    for(ri in 1:max(rind))
    {
      tdat1=as.vector(b$data[rind==ri, ci])
      bfc[ci] = bfc[ci] + lognormal(tdat1, sig)
    }
    bfc[ci] = bfc[ci] - lognormal(tdat, sig)
  }
  ##################################################################################
  
  ##################################################################################
  par(mfrow=c(2,2))
  
  BFrow=sort(bfr, method = "shell", index.return = TRUE)
  # if(is.null(b$row_name))
  #   rname = BFrow$ix
  # else
    rname = b$row_name[BFrow$ix]
  barplot(BFrow$x, names.arg = rname, las=2, col = heat.colors(length(rind)), main = "rows impact", ylab = "Log Bayes factor")
  
  BFrowc=sort(rlc, method = "shell", index.return = TRUE)
  # if(is.null(b$row_name))
  #   rname = BFrowc$ix
  # else
    rname = b$row_name[BFrowc$ix]
  barplot(BFrowc$x, names.arg = rname, las=2, col = heat.colors(length(rind)), main = "row clusters impact", ylab = "Log Bayes factor")
  
  BFcol=sort(bfc, method = "shell", index.return = TRUE)
  # if(is.null(b$col_name))
  #   cname = BFcol$ix
  # else
    cname = b$col_name[BFcol$ix]
  barplot(BFcol$x, names.arg = cname, las=2, col = heat.colors(length(cind)), main = "cols impact", ylab = "Log Bayes factor")
  
  BFcolc=sort(clc, method = "shell", index.return = TRUE)
  # if(is.null(b$col_name))
  #   cname = BFcolc$ix
  # else
    cname = b$col_name[BFcolc$ix]
  barplot(BFcolc$x, names.arg = cname, las=2, col = heat.colors(length(cind)), main = "col clusters impact", ylab = "Log Bayes factor")
  
  return(list("row_likelihood"=rbind(bfr, rlc), "col_likelihood"=rbind(bfc, clc)))
}
