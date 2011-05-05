\name{lbsGetInfoDocuments}
\alias{lbsGetInfoDocuments}
\title{Retrieve document information}
\usage{lbsGetInfoDocuments(conn, idDocuments)}
\description{Retrieves information on given documents.}
\value{A list of \code{docinfo} objects, that is lists with the following components:
\itemize{
\item \code{IdDocument} --- numeric; document identifier in the table \code{Biblio_Documents},
\item \code{Authors} --- list of \code{authorinfo} objects (see e.g. \code{\link{as.character.authorinfo}}).
\item \code{Title} --- title of the document,
\item \code{BibEntry} --- bibliographic entry,
\item \code{UniqueId} --- unique character identifier,
\item \code{Pages} --- number of pages,
\item \code{Citations} --- number of citations,
\item \code{Year} --- publication year,
\item \code{Type} --- document type, e.g. \code{Article} or \code{Conference Paper}.
}}
\seealso{\code{\link{print.docinfo}}, \code{\link{lbsSearchDocuments}},
\code{\link{lbsGetInfoAuthors}},\cr
\code{\link{as.character.authorinfo}}, \code{\link{as.character.docinfo}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{idDocuments}{a numeric or integer vector with document identifiers (see column \code{IdDocument} in the table \code{Biblio_Documents}).}}
\examples{\dontrun{
conn <- dbBiblioConnect("Bibliometrics.db");
## ...
id <- lbsSearchDocuments(conn,
idAuthors=lbsSearchAuthors(conn, "Knuth\%"));
lbsGetInfoDocuments(conn, id);
## ...}}
