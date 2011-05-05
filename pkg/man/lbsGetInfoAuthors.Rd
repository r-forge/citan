\name{lbsGetInfoAuthors}
\alias{lbsGetInfoAuthors}
\title{Retrieve author information}
\usage{lbsGetInfoAuthors(conn, idAuthors)}
\description{Retrieves basic information on given authors.}
\value{A list of \code{authorinfo} objects, that is lists with the following components:
\itemize{
\item \code{IdAuthor} --- numeric; author's identifier in the table \code{Biblio_Authors},
\item \code{Name} --- character; author's name.
}}
\seealso{\code{\link{lbsSearchAuthors}}, \code{\link{lbsSearchDocuments}},
\code{\link{lbsGetInfoDocuments}},\cr
\code{\link{as.character.authorinfo}}, \code{\link{print.authorinfo}},}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{idAuthors}{a numeric or integer vector with author identifiers (see column \code{IdAuthor} in the table \code{Biblio_Authors}).}}
\examples{\dontrun{
#' conn <- dbBiblioConnect("Bibliometrics.db");
## ...
id <- lbsSearchAuthors(conn, c("Smith\%", "Knuth D.E.", "V_n \%"));
lbsGetInfoAuthors(conn, id);
## ...}}
