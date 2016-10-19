\name{hbiclust-package}
\alias{hbiclust-package}
\alias{hbiclust}

\docType{package}

\title{ Hierarchical biclustering }

\description{ Plot a 2D or 3D hierarchical biclustering with respectively, hbiclust.plot and forestogram.}

\details{
TESTING DESCRIPTION.
}

\author{ Mohammad Sajjad Ghaemi }

\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
% ~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
set.seed(150)
row=3
col=3

a=cbind2(matrix( rnorm(row*col,mean=0,sd=1), row, col) , matrix( rnorm(row*col,mean=5,sd=1), row, col));
a=cbind2(a, matrix( rnorm(row*col,mean=5,sd=1), row, col));
b=cbind2(matrix( rnorm(row*col,mean=-15,sd=1), row, col) , matrix( rnorm(row*col,mean=0,sd=1), row, col));
b=cbind2(b, matrix( rnorm(row*col,mean=15,sd=1), row, col));
d=cbind2(matrix( rnorm(row*col,mean=-25,sd=1), row, col) , matrix( rnorm(row*col,mean=0,sd=1), row, col));
d=cbind2(d, matrix( rnorm(row*col,mean=25,sd=1), row, col));
c=rbind2(a,b);
c=rbind2(c,d);
row=9
col=9
b<-hbiclust(c, method="ward")
forestogram(b, cut_height = b$hcut, 
            line_width = 3.0, 
            interpolate_tree_colors = FALSE, 
            cut_base_alpha = 0.4,
            draw_only_from_cut =FALSE);
cutforest(b, k=9)
hbiclust.plot(b, cut_height = b$hcut);
}
