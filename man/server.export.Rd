\name{server.export}
\alias{server.export}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Server tools: export data.frame}

\description{
  Export a data.frame in CSV, XLSX or RDS format and create a html link on the output.
}
\usage{
server.export(x, filename=NULL, title=NULL, 
              format=c("csv","xlsx","rds"),
              rownames=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ - a data.frame}
  \item{filename}{ - a string, the file name without extension}
  \item{Title}{ - a string, the text of the link}
  \item{format}{ - a string, the file format ["csv","xlsx","rds"]}
  \item{rownames}{ - boolean, TRUE if row.names are exported. Default FALSE}
}
\details{
Files are exported in the subdirectory "exports". If this directory does
not exists, it will be created.
}

\value{Return nothing. GDS the Global DataSet is modified}

\references{}

\author{jp.decorps@epiconcept.fr}
\note{
The chunk where the function is called in, must have option "echo = asis"
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{}

\examples{
data(iris)
server.export(iris, filename = "iris",title = "CSV: Data.frame iris")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
