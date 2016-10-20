\name{figure.export}
\alias{figure.export}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Server tools: export data.frame}

\description{
  Export a graphic in JPEG, PNG, PDF or SVG format and create a html link on the output.
}
\usage{
figure.export(x, filename=NULL, title=NULL, 
              format=c("png","pdf","jpg","svg")
              plot.width=1120,
              plot.height=630) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ - NULL for a standard plot, ggplot2 otherwise}
  \item{filename}{ - a string, the file name without extension}
  \item{Title}{ - a string, the text of the link}
  \item{format}{ - a vector string, the file format c("png","pdf","jpg","csv")}
  \item{plot.width}{ - an integer, width of plot, only used by standard plots not ggplot}
  \item{plot.height}{ - an integer, height of plot, only used by standard plots not ggplot}
}
\details{
Files are exported in the subdirectory "exports". If this directory does
not exists, it will be created. You can export in multiple formats at the same time.
}

\value{Return nothing.}

\references{}

\author{jp.decorps@epiconcept.fr}
\note{
The chunk where the function is called in, must have option "echo = asis"
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{}

\examples{
data(iris)
plot(iris$Sepal.Length, iris$Sepal.Width)
figure.export(NULL, filename = "iris",title = "Fig_1", c("jpg", "pdf"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
