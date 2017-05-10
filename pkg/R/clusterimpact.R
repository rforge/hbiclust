clusterimpact <- function(b, rind, cind)
{
  sswcc = rep(0, length(cind))
  ssbcc = rep(0, length(cind))
  
  for(ci in 1:max(cind))
  {
    for(ri in 1:max(rind))
    {
      tdat1=as.vector(b$data[rind==ri, cind==ci])
      sswcc[ci] = sswcc[ci] + sum( ( tdat1-mean(tdat1) )^2 )
    }
    tdat=as.vector(b$data[, cind==ci])
    ssbcc[ci] = sum( (tdat-mean(tdat))^2 )-sswcc[ci]
  }
  clc=cind
  for(ci in 1:max(cind))
  {
   clc[cind==ci]=ssbcc[ci]
  }
  ###########################################################
  sswcr = rep(0, length(rind))
  ssbcr = rep(0, length(rind))
  
  for(ri in 1:max(rind))
  {
    for(ci in 1:max(cind))
    {
      tdat1=as.vector(b$data[rind==ri, cind==ci])
      sswcr[ri] = sswcr[ri] + sum( ( tdat1-mean(tdat1) )^2 )
    }
    tdat=as.vector(b$data[rind==ri,])
    ssbcr[ri] = sum( (tdat-mean(tdat))^2 )-sswcr[ri]
  }
  rlc=rind
  for(ri in 1:max(rind))
  {
   rlc[rind==ri]=ssbcr[ri]
  }
  ###################################################################
  sswr = rep(0, length(rind))
  ssbr = rep(0, length(rind))
  for(ri in 1:length(rind))
  {
    tdat=as.vector(b$data[ri, ])
    for(ci in 1:max(cind))
    {
      tdat1=as.vector(b$data[ri, cind==ci])      
      sswr[ri] = sswr[ri] + sum( ( tdat1-mean(tdat1) )^2 )
    }
    ssbr[ri] = sum( (tdat-mean(tdat))^2 )-sswr[ri]
  }  
    
  sswc = rep(0, length(cind))
  ssbc = rep(0, length(cind))
  for(ci in 1:length(cind))
  {
    tdat=as.vector(b$data[, ci])
    for(ri in 1:max(rind))
    {
      tdat1=as.vector(b$data[rind==ri, ci])
      sswc[ci] = sswc[ci] + sum( ( tdat1-mean(tdat1) )^2 )
    }
    ssbc[ci] = sum( (tdat-mean(tdat))^2 )-sswc[ci]
  }
  #return(list("max_row"=rbind(max(rl), max(rlc)),"row_likelihood"=rbind(rl/max(rl), rlc/max(rlc)), "max_col"=rbind(max(cl), max(clc)), "col_likelihood"=rbind(cl/max(cl), clc/max(clc))))
  par(mfrow=c(2,2))
  
  bfr=ssbr/sum(ssbr)
  BFrow=sort(bfr, method = "shell", index.return = TRUE)
  #if(is.null(b$row_name))
  #  rname = BFrow$ix
  #else
    rname = b$row_name[BFrow$ix]
  barplot(BFrow$x, names.arg = rname, las=2, col = heat.colors(length(rind)), main = "rows impact", ylab = "between sum of square")
  
  rlc=rlc/sum(unique(rlc))
  BFrowc=sort(rlc, method = "shell", index.return = TRUE)
  #if(is.null(b$row_name))
  #  rname = BFrowc$ix
  #else
    rname = b$row_name[BFrowc$ix]
  barplot(BFrowc$x, names.arg = rname, las=2, col = heat.colors(length(rind)), main = "row clusters impact", ylab = "between sum of square")
  
  bfc=ssbc/sum(ssbc)
  BFcol=sort(bfc, method = "shell", index.return = TRUE)
  #if(is.null(b$col_name))
  #  cname = BFcol$ix
  #else
    cname = b$col_name[BFcol$ix]
  barplot(BFcol$x, names.arg = cname, las=2, col = heat.colors(length(cind)), main = "cols impact", ylab = "between sum of square")
  
  clc=clc/sum(unique(clc))
  BFcolc=sort(clc, method = "shell", index.return = TRUE)
  #if(is.null(b$col_name))
  #  cname = BFcolc$ix
  #else
    cname = b$col_name[BFcolc$ix]
  barplot(BFcolc$x, names.arg = cname, las=2, col = heat.colors(length(cind)), main = "col clusters impact", ylab = "between sum of square")
  
  return(list("row_likelihood"=rbind(ssbr, rlc), "col_likelihood"=rbind(ssbc, clc)))
}
