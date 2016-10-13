ColClusterEffect <- function(b, rind, cind)
{
  for(ci in 1:max(cind))
  {
    lkc = c(1:max(cind))
    for(ri in 1:max(rind))
    {
      tdat=b$data[rind==ri, cind==ci];
      if(class(tdat)=="matrix")                    
      {
        mns = rowMeans(tdat);
        lkc[ci] = lkc[ci]+(sum(sweep(tdat, 1, mns)^2)/ncol(tdat));
      }
    }
    tdat=b$data[cind==ci];
    if(class(tdat)=="matrix")                    
    {
      mns = rowMeans(tdat);
      lkc[ci] = lkc[ci]-(sum(sweep(tdat, 1, mns)^2)/ncol(tdat));
    }
  }
  return(lkc)
}