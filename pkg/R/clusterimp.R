clusterimp <- function(b, rind, cind)
{
  lkc = rep(0, max(cind))
  for(ci in 1:max(cind))
  {
    for(ri in 1:max(rind))
    {
      tdat=b$data[rind==ri, cind==ci];
      if(class(tdat)=="matrix")                    
      {
        mns = colMeans(tdat);
        lkc[ci] = lkc[ci]+(sum(sweep(tdat, 2, mns)^2)/nrow(tdat));
      }
    }
    tdat=b$data[, cind==ci];
    if(class(tdat)=="matrix")                    
    {
      mns = colMeans(tdat);
      lkc[ci] = lkc[ci]-(sum(sweep(tdat, 2, mns)^2)/nrow(tdat));
    }
  }
  
  lkr = rep(0, max(rind))
  for(ri in 1:max(rind))
  {
    for(ci in 1:max(cind))
    {
      tdat=b$data[rind==ri, cind==ci];
      if(class(tdat)=="matrix")                    
      {
        mns = rowMeans(tdat);
        lkr[ri] = lkr[ri]+(sum(sweep(tdat, 1, mns)^2)/ncol(tdat));
      }
    }
    tdat=b$data[rind==ri,];
    if(class(tdat)=="matrix")
    {
      mns = rowMeans(tdat);
      lkr[ri] = lkr[ri]-(sum(sweep(tdat, 1, mns)^2)/ncol(tdat));
    }
  }
  mns = rowMeans(b$data);
  rl=rowSums(sweep(b$data, 1, mns)^2)/ncol(b$data);
  rlc=rind
  for(ri in 1:max(rind))
  {
    rlc[rlc==ri]=-lkr[ri]
  }
  
  mns = colMeans(b$data);
  cl=colSums(sweep(b$data, 2, mns)^2)/nrow(b$data);
  clc=cind
  for(ci in 1:max(cind))
  {
    clc[clc==ci]=-lkc[ci]
  }
  
  return(list("Max row"=rbind(max(rl), max(rlc)),"Row likelihood"=rbind(rl/max(rl), rlc/max(rlc)), "Max col"=rbind(max(cl), max(clc)), "Col likelihood"=rbind(cl/max(cl), clc/max(clc))))
}
