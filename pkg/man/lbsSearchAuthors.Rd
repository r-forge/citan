\name{lbsSearchAuthors}
\alias{lbsSearchAuthors}
\title{Find authors meeting given criteria}
\usage{lbsSearchAuthors(conn, names.like)}
\description{Finds authors by name.}
\details{\code{names.like} is a set of search patterns in an SQL \code{LIKE} format,
i.e. an underscore \code{_} matches a single character and a percent sign
\code{\%} matches any set of characters. The search is case-insensitive.}
\value{Integer vector of authors' identifiers which match at least one of given 
SQL-LIKE patterns.}
\seealso{\code{\link{lbsGetInfoAuthors}},
\code{\link{lbsSearchDocuments}},
\code{\link{lbsGetInfoDocuments}},\cr
\code{\link{lbsFindDuplicateAuthors}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{names.like}{a character vector of SQL-LIKE patterns to match authors' names.}}
\examples{\dontrun{
conn <- dbBiblioConnect("Bibliometrics.db");
## ...
id <- lbsSearchAuthors(conn, c("Smith\%", "Knuth D.E.", "V_n \%"));
lbsGetInfoAuthors(conn, id);
## ...}}
