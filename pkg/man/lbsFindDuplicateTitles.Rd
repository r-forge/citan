\name{lbsFindDuplicateTitles}
\alias{lbsFindDuplicateTitles}
\title{Suggest documents to be merged (**EXPERIMENTAL**)}
\usage{lbsFindDuplicateTitles(conn, surveyDescription, ignoreTitles.like,
    aggressiveness=1)}
\description{This function suggests the user some groups of documents that possibly
should be merged.
It is based on documents' titles similarity comparisons.
It uses a heuristic algorithm, which behavior
is controlled by the \code{aggressiveness} parameter.}
\details{The search results are presented in a convenient-to-use graphical dialog box.
The function tries to order the groups of documents according
to their relevance (**EXPERIMENTAL** algorithm).
Note that the calculation may take a few minutes!

\code{ignoreTitles.like} is a set of search patterns in an SQL \code{LIKE} format,
i.e. an underscore \code{_} matches a single character and a percent sign
\code{\%} matches any set of characters. The search is case-insensitive.}
\value{A numeric vector of user-selected documents' identifiers to be removed.}
\seealso{\code{\link{lbsDeleteDocuments}}, \code{\link{lbsFindDuplicateAuthors}}, \code{\link{lbsGetInfoDocuments}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{surveyDescription}{single character string or \code{NULL}; survey description to restrict to or \code{NULL}.}
\item{ignoreTitles.like}{a character vector of SQL-LIKE patterns to match documents' titles to be ignored or \code{NULL}.}
\item{aggressiveness}{nonnegative integer; \code{0} for showing only exact matches;
the higher the value, the more documents will be proposed.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
listdoc <- lbsFindDuplicateTitles(conn,
ignoreTitles.like=c("\%In this issue\%", "\%Editorial", "\%Introduction",
"Letter to \%", "\%Preface"),
aggressiveness=2);
lbsDeleteDocuments(conn, listdoc);
dbCommit(conn);
## ...}}
