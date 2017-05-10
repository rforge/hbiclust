empericalsig <- function(b, rind, cind)
{
  sig=0
  for(ci in 1:max(cind))
  {
    for(ri in 1:max(rind))
    {
      tdat=as.vector(b$data[rind==ri, cind==ci])
      sig = sig+ sum((tdat-mean(tdat))^2)
    }
  }
  return(sig/length(b$data))
}