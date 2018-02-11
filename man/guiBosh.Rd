\name{guiBosh}
\alias{guiBosh}
\alias{guiExecBosh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Modified gui and guiExec functions
}
\description{
  Modified gui and guiExec functions.
}
\usage{
guiBosh(func, argOption = NULL, argFilename = NULL, argList = NULL,
        argSlider = NULL, argCommand = NULL, argEdit = NULL,
        argFilter = NULL, argText = NULL, argType = NULL,
        argGridOrder = 1:length(formals(func)),
        argGridSticky = rep("a", length(formals(func))),
        argGridFrame = rep("f", length(formals(func))), title = NULL,
        exec = "OK", closeOnExec = is.null(output),
        cancelButton = TRUE, callback = NULL, output = "m",
        helps = "auto", helpsFunc = NULL, grid = TRUE, modal = NULL,
        nameFix = TRUE, getFix = TRUE, verbose = FALSE)

guiExecBosh(lastTouched = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{func}{see the help page of \code{gui} in package \pkg{fgui}.}
  \item{argOption}{ --ditto-- }
  \item{argFilename}{ --ditto-- }
  \item{argList}{ --ditto-- }
  \item{argSlider}{ --ditto-- }
  \item{argCommand}{ --ditto-- }
  \item{argEdit}{ --ditto-- }
  \item{argFilter}{ --ditto-- }
  \item{argText}{ --ditto-- }
  \item{argType}{ --ditto-- }
  \item{argGridOrder}{ --ditto-- }
  \item{argGridSticky}{ --ditto-- }
  \item{argGridFrame}{ --ditto-- }
  \item{title}{ --ditto-- }
  \item{exec}{ --ditto-- }
  \item{closeOnExec}{ --ditto-- }
  \item{cancelButton}{ --ditto-- }
  \item{callback}{ --ditto-- }
  \item{output}{ --ditto-- }
  \item{helps}{ --ditto-- }
  \item{helpsFunc}{ --ditto-- }
  \item{grid}{ --ditto-- }
  \item{modal}{ --ditto-- }
  \item{nameFix}{ --ditto-- }
  \item{getFix}{ --ditto-- }
  \item{verbose}{ --ditto-- }
  \item{lastTouched}{see the help page of \code{guiExec} in package
    \pkg{fgui}.}
}
\details{

  These functions are taken from package \code{fgui} and slightly
  modified. \code{guiBosh} is a modification of \code{gui},
  \code{guiExecBosh} of \code{guiExec}.

  The main change is that the parsing of the help information is changed
  completely. The other changes are very minor: some messages from these
  functions are disabled since the user sees them too late (after the
  GUI window is closed) and several \code{require} commands are removed.

  Both functions call internally unexported functions from \code{fgui}.

}
%\value{
%%%  ~Describe the value returned
%%%  If it is a LIST, use
%%%  \item{comp1 }{Description of 'comp1'}
%%%  \item{comp2 }{Description of 'comp2'}
%%% ...
%}
\references{
%% ~put references to the literature/web site here ~
  Thomas J. Hoffmann, Nan M. Laird (2009). fgui: A Method for
  Automatically Creating Graphical User Interfaces for Command-Line R
  Packages. Journal of Statistical Software 30(2), 1-14. URL
  http://www.jstatsoft.org/v30/i02/.
}
\author{small modifications by Georgi N. Boshnakov, author of the
  originals: Thomas J. Hoffmann}
%\note{
%%%  ~~further notes~~
%}
%
%%% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%% \examples{
% ##---- Should be DIRECTLY executable !! ----
% }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{gui}
