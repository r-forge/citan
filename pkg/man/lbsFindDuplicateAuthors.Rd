\name{lbsFindDuplicateAuthors}
\alias{lbsFindDuplicateAuthors}
\title{Suggest groups of authors to be merged (**EXPERIMENTAL**)}
\usage{lbsFindDuplicateAuthors(conn, names.like, ignoreWords=c("van", "von",
    "der", "no", "author", "name", "available"), minWordLength=4,
    orderResultsBy=c("citations", "ndocuments", "name"),
    aggressiveness=0)}
\description{This function suggests the user some groups of authors that possibly should be merged.
It bases on authors' names similarity comparisons.}
\details{It uses a heuristic **EXPERIMENTAL** algorithm, which behavior
is controlled by the \code{aggressiveness} parameter.

The search results are presented in a convenient-to-use graphical dialog box.
Note that the calculation may take a few minutes!

\code{names.like} is a set of search patterns in an SQL \code{LIKE} format,
i.e. an underscore \code{_} matches a single character and a percent sign
\code{\%} matches any set of characters. The search is case-insensitive.}
\value{List of authors' identifiers to be merged.
The first element of each vector is the one marked by the user as \emph{Parent},
and the rest are the \emph{Children}.}
\seealso{\code{\link{lbsMergeAuthors}}, \code{\link{lbsFindDuplicateTitles}}, \code{\link{lbsGetInfoAuthors}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{names.like}{a character vector of SQL-LIKE patterns that allow for restricting
the search procedure to only given authors' names.}
\item{ignoreWords}{character vector; words to be ignored.}
\item{minWordLength}{numeric; minimal word length to be considered.}
\item{orderResultsBy}{determines results' presentation order; one of
\code{citations}, \code{ndocuments} \code{name}.}
\item{aggressiveness}{nonnegative integer; controls the search depth.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
listauth <- lbsFindDuplicateAuthors(conn,
ignoreWords=c("van", "von", "der", "no", "author", "name", "available"),
minWordLength=4,
orderResultsBy=c("citations"),
aggressiveness=1);
lbsMergeAuthors(conn, listauth);
dbCommit(conn);
## ...}}
