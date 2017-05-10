lognormal <- function(data, sig)
{
  n=length(data)
  #ret = -n*log(pi)/2 -n*log(var(data))/2 - n/2
  ret =  -(sum( (data-mean(data) )^2 ))/(sig)
  return(ret)
}