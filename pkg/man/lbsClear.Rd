\name{lbsClear}
\alias{lbsClear}
\title{Clear a Local Bibliometric Storage}
\usage{lbsClear(conn, verbose=TRUE)}
\description{Clears a Local Bibliometric Storage by dropping all tables
named \code{Biblio_*} and all views named \code{ViewBiblio_*}.}
\details{For safety reasons, an SQL transaction  opened at the beginning of the
removal process is not committed (closed) automatically.
You should do it on your own (or rollback it), see Examples below.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{lbsConnect}}, \code{\link{lbsCreate}},
\code{\link{Scopus_ImportSources}}, \code{\link{lbsDeleteAllAuthorsDocuments}}
\code{\link{dbCommit}}, \code{\link{dbRollback}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{verbose}{logical; \code{TRUE} to inform about the progress of database contents' removal.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
lbsClear(conn);
dbCommit(conn);
lbsCreate(conn);
Scopus_ImportSources(conn);
## ...
dbDisconnect(conn);}}
