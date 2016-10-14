ColClusterEffect <- function(b, rind, cind)
{
  lkc = rep(0, max(cind))
  for(ci in 1:max(cind))
  {
    #print(ci)
    for(ri in 1:max(rind))
    {
      #print(ri)
      tdat=b$data[rind==ri, cind==ci];
      if(class(tdat)=="matrix")                    
      {
        mns = rowMeans(tdat);
        lkc[ci] = lkc[ci]+(sum(sweep(tdat, 1, mns)^2)/nrow(tdat));
      }
      #print((sum(sweep(tdat, 1, mns)^2)/nrow(tdat)))
    }
    tdat=b$data[, cind==ci];
    if(class(tdat)=="matrix")                    
    {
      mns = colMeans(tdat);
      lkc[ci] = lkc[ci]-(sum(sweep(tdat, 2, mns)^2)/ncol(tdat));
    }
    #print((sum(sweep(tdat, 2, mns)^2)/ncol(tdat)))
  }
  return(-lkc)
}
