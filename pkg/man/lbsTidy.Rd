\name{lbsTidy}
\alias{lbsTidy}
\title{Tidy a Local Bibliometric Storage}
\usage{lbsTidy(conn)}
\description{Tidies up a Local Bibliometric Storage
by removing all authors with no documents and executing the \code{VACUUM}
SQL command.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{lbsConnect}}, \code{\link{lbsCreate}},
\code{\link{Scopus_ImportSources}},
\code{\link{lbsDeleteAllAuthorsDocuments}},
\code{\link{dbCommit}}, \code{\link{dbRollback}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}}
