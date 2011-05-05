\name{lbsDeleteAllAuthorsDocuments}
\alias{lbsDeleteAllAuthorsDocuments}
\title{Delete all authors, documents and surveys from a Local Bibliometric Storage}
\usage{lbsDeleteAllAuthorsDocuments(conn, verbose=TRUE)}
\description{Deletes are author, document and survey information from a Local Bibliometric
Storage.}
\details{For safety reasons, an SQL transaction opened at the beginning of the
removal process is not committed (closed) automatically.
You should do it on your own (or rollback it), see Examples below.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{lbsClear}}, \code{\link{dbCommit}}, \code{\link{dbRollback}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{verbose}{logical; \code{TRUE} to print out the progress of database contents' removal.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
lbsDeleteAllAuthorsDocuments(conn);
dbCommit(conn);
## ...
dbDisconnect(conn);}}
