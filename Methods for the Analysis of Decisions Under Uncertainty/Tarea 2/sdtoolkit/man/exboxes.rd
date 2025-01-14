\name{exboxes}
\alias{exboxes}
\docType{data}
\title{Example Box Sequence Object}
\description{
An example box sequence for demonstrating \pkg{sdtoolkit} functions that apply to box sequence objects.  As of now these other functions are \code{seqinfo} and \code{dimplot}.
}
\usage{data(exboxes)}
\format{
  The format is that of a box sequence object as output by \code{sdprim}.  Please the \sQuote{Value} section of the documentation for \code{\link{sdprim}} for additional details.
}
%\details{
%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
The object was generated by running \code{sdprim} on the common dataset \code{quakes}, with the fourth varable (\code{mag}) used as the output variable, thresholded at 5.0. 
}
%\references{
%  ~~ possibly secondary sources and usages ~~
%}
\examples{
data(exboxes)
}
\keyword{datasets}
