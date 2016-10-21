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
  clc=cind
  for(ci in 1:max(cind))
  {
   clc[clc==ci]=-lkc[ci]
  }
  ###########################################################
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
  
  rlc=rind
  for(ri in 1:max(rind))
  {
   rlc[rlc==ri]=-lkr[ri]
  }
  ###################################################################
  rl = rep(0, length(rind))
  for(ri in 1:length(rind))
  {
    for(ci in 1:max(cind))
    {
      tdat=b$data[ri, cind==ci];
      rl[ri] = rl[ri]+var(tdat);
    }
    tdat=b$data[ri,];
    rl[ri] = rl[ri]-var(tdat)
  }
  rl = -rl
  
  cl = rep(0, length(cind))
  for(ci in 1:length(cind))
  {
    for(ri in 1:max(rind))
    {
      tdat=b$data[rind==ri, ci];
      cl[ci] = cl[ci]+var(tdat);
    }
    tdat=b$data[,ci];
    cl[ci] = cl[ci]-var(tdat)
  }
  cl = -cl
  # mns = colMeans(b$data);
  # cl=colSums(sweep(b$data, 2, mns)^2)/nrow(b$data);
  # clc=cind
  # for(ci in 1:max(cind))
  # {
  #   clc[clc==ci]=-lkc[ci]
  # }
  # 
  #return(list("max_row"=rbind(max(rl), max(rlc)),"row_likelihood"=rbind(rl/max(rl), rlc/max(rlc)), "max_col"=rbind(max(cl), max(clc)), "col_likelihood"=rbind(cl/max(cl), clc/max(clc))))
  return(list("row_likelihood"=rbind(rl, rlc), "col_likelihood"=rbind(cl, clc)))
}
