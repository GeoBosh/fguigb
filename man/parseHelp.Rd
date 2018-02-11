\name{parseHelp}
\alias{parseHelp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Extract argument descriptions from the documentation of a
  function}
\description{Parse the documentation of an object to extract argument
  descriptions.}
\usage{
parseHelp(func)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func}{a function name or function.}
}
\details{
  This function is a replacement of the function with the same name in
  package \pkg{fgui} with improved parsing based on the new
  (R>=2.10) help system.

  This version of \code{parseHelp} detects when multiple arguments are
  described in one entry and allocates that entry to each of them,
  see the example with \code{seq.default} which describes \code{from,to}
  together.
}
\value{
  A named list with one entry (a string) for each argument of \code{f}.
}
%\references{
%%% ~put references to the literature/web site here ~
%}
\author{Georgi N. Boshnakov}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
parseHelp("rnorm")
parseHelp("seq")
# fgui:::parseHelp("seq")  # for comparison
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{help}
\keyword{Rd}
