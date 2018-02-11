\name{fguigb-package}
\alias{fguigb-package}
\alias{fguigb}
\docType{package}
\title{
  \Sexpr[stage=build,results=hide]{pd <- packageDescription("fguigb")}
  \Sexpr[stage=build,results=hide]{lb <- library(help="fguigb", character.only=TRUE)}
  \Sexpr[stage=build]{pd$Title}
}
\description{
  \Sexpr[stage=build]{pd$Description}
}
\details{
   \tabular{ll}{
   Package:  \tab \Sexpr[stage=build]{pd$Package}\cr
   Type:     \tab \Sexpr[stage=build]{pd$Type}\cr
   Version:  \tab \Sexpr[stage=build]{pd$Version} \cr
   Date:     \tab \Sexpr[stage=build]{pd$Date}\cr
   License:  \tab \Sexpr[stage=build]{pd$License}\cr
   LazyLoad: \tab \Sexpr[stage=build]{pd$LazyLoad}\cr
   Built:    \tab \Sexpr[stage=build]{pd$Built}\cr
   }

   Index:
  \Sexpr[stage=build,results=rd]{paste("\\\\preformatted{", paste(if(!is.null(lb$info[[2]])) lb$info[[2]] else "No entries",collapse="\n"), "}", sep="")}

% ~~ An overview of how to use the package, including the most important ~~
% ~~ functions ~~

This package provides the function \code{\link{my.gui.fun}}. It creates
a GUI window for a function, which allows the user to try various values
for the arguments, see the command that will be run and, if desired, run
it on exit. For each argument, its help text from the documentation page
of the function is provided, the help text may even be from another
function.

The GUI interface is created with functions from package \pkg{fgui} by
Thomas J. Hoffmann, but (mostly) to incorporate processing of the recent
(post R-2.10) help format I use modifications of some of the top level
functions in \code{fgui}, see \code{\link{guiBosh}} and
\code{\link{guiExecBosh}}.


}
\author{
  \Sexpr[stage=build]{pd$Author}

Maintainer: \Sexpr[stage=build]{pd$Maintainer}
}
\references{
% bibentry:all
  Thomas J. Hoffmann, Nan M. Laird (2009). fgui: A Method for
  Automatically Creating Graphical User Interfaces for Command-Line R
  Packages. Journal of Statistical Software 30(2), 1-14. URL
  http://www.jstatsoft.org/v30/i02/.
}
% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
% ~~ the R documentation directory ~~
\keyword{ package }
\keyword{gui}
%\seealso{
% ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
\examples{
% ~~ simple examples of the most important functions ~~
# a bare minimum for read.table, still useful as
# we often forget to set the header argument as needed.
#
# my.gui.fun("read.table", alist(file= , header=TRUE))

# with more arguments presented...
# my.gui.fun("read.table",alist(file=,header=TRUE,sep="",quote="\"'"))

}
