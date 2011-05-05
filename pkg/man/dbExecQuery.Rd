\name{dbExecQuery}
\alias{dbExecQuery}
\title{Execute a query and free its resources}
\usage{dbExecQuery(conn, statement, rollbackOnError=FALSE)}
\description{Executes an SQL query and immediately frees all allocated resources.}
\details{This function is useful in executing queries like \code{CREATE TABLE},
\code{UPDATE}, \code{INSERT}, etc.

It has its own exception handler, which prints out detailed description
of caught error details.}
\seealso{\code{\link{dbSendQuery}}, \code{\link{dbClearResult}}, \code{\link{dbGetQuery}}}
\arguments{\item{conn}{a \code{DBI} connection object.}
\item{statement}{a character string with the SQL statement to be executed.}
\item{rollbackOnError}{logical; if \code{TRUE}, then the function executes rollback on current transaction if an exception occurs.}}
