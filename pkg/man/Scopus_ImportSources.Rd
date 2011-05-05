\name{Scopus_ImportSources}
\alias{Scopus_ImportSources}
\title{Import SciVerse Scopus coverage information and ASJC codes to a Local Bibliometric Storage}
\usage{Scopus_ImportSources(conn, impactColumn, verbose=T)}
\description{Imports \emph{SciVerse Scopus} covered titles and their ASJC codes to an empty Local Bibliometric Storage (\acronym{LBS}).}
\details{This routine should be called prior to importing any document information
to the LBS with the function \code{\link{lbsImportDocuments}}.

If multiple sources with the same ISSN (either Print-ISSN or E-ISSN)
are found in \code{\link{Scopus_SourceList}}
then only the first matching record is added to the table \code{Biblio_Sources}.
Additionally, a warning is generated.

Note that adding all the sources takes some time.

Only basic ASJC and \emph{SciVerse Scopus} source information
retrieved from \code{\link{Scopus_ASJC}} (stored in the table \code{Biblio_Categories})
and \code{\link{Scopus_SourceList}} (stored in tables \code{Biblio_Countries} and \code{Biblio_Sources})
will be added to the LBS.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}}, \code{\link{Scopus_ReadCSV}}, \code{\link{lbsConnect}}, \code{\link{lbsCreate}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{impactColumn}{single character value determining column name in \code{\link{Scopus_SourceList}} with values of some source impact measurements to be imported, e.g. \code{"SJR_2009"}, or \code{NULL} if no such data should be saved.}
\item{verbose}{logical; \code{TRUE} to inform about the progress of the process.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
lbsCreate(conn);
Scopus_ImportSources(conn);
## ...
dbDisconnect(conn);}}
