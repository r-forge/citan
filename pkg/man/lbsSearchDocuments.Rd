\name{lbsSearchDocuments}
\alias{lbsSearchDocuments}
\title{Find documents meeting given criteria}
\usage{lbsSearchDocuments(conn, titles.like, idAuthors, citations.expr,
    pages.expr, year.expr, documentTypes, surveyDescription)}
\description{Searches for documents meeting given criteria (e.g. document titles,
documents' authors identifiers, number of citations, number of pages, publication years
or document types).}
\details{\code{titles.like} is a set of search patterns in an SQL \code{LIKE} format,
i.e. an underscore \code{_} matches a single character and a percent sign
\code{\%} matches any set of characters. The search is case-insensitive. 

The expressions passed as
parameters \code{citations.expr}, \code{pages.expr}, \code{year.expr}
must be acceptable by SQL WHERE clause in the form
\code{WHERE field <expression>}, see Examples below.}
\value{Integer vector of documents' identifiers matching given criteria.}
\seealso{\code{\link{lbsGetInfoAuthors}},
\code{\link{lbsSearchAuthors}},
\code{\link{lbsGetInfoDocuments}},\cr
\code{\link{lbsFindDuplicateTitles}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{titles.like}{a character vector of SQL-LIKE patterns to match documents' titles or \code{NULL}.}
\item{idAuthors}{a numeric or integer vector with author identifiers (see column \code{IdAuthor} in the table \code{Biblio_Authors}) or \code{NULL}.}
\item{citations.expr}{an expression determining the desired number of citations or \code{NULL}, see Examples below.}
\item{pages.expr}{an expression determining the desired number of pages or \code{NULL}, see Examples below.}
\item{year.expr}{an expression determining the desired publication year or \code{NULL}, see Examples below.}
\item{documentTypes}{character vector or \code{NULL}; specifies document types to restrict to;
a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
\code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
\code{Short Survey}. \code{NULL} means no such restriction.}
\item{surveyDescription}{single character string or \code{NULL}; survey description to restrict to or \code{NULL}.}}
\examples{\dontrun{
conn <- dbBiblioConnect("Bibliometrics.db");
## ...
idd <- lbsSearchDocuments(conn, pages.expr=">= 400",
year.expr="BETWEEN 1970 AND 1972");
lbsGetInfoDocuments(conn, idd);
## ...}}
