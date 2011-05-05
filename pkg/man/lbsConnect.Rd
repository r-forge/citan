\name{lbsConnect}
\alias{lbsConnect}
\title{Connect to a Local Bibliometric Storage}
\usage{lbsConnect(dbfilename)}
\description{Connects to a Local Bibliometric Storage handled by the SQLite engine
(see \pkg{RSQLite} package documentation).}
\details{Do not forget to close the connection (represented by the object returned)
with \code{\link{dbDisconnect}} after use.
Note that you may freely access the database using
functions from the \pkg{DBI} package called on the
returned connection object.}
\value{An object of type \code{SQLiteConnection}.
It is used to direct commands to the SQLite engine.}
\seealso{\code{\link{lbsCreate}}, \code{\link{dbDisconnect}}}
\arguments{\item{dbfilename}{name of the file storing the SQLite database.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
dbDisconnect(conn);}}
