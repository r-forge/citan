\name{Scopus_SourceList}
\alias{Scopus_SourceList}
\title{Scopus list of covered sources}
\description{List of Elsevier's \emph{SciVerse Scopus} covered titles (journals, conference proceedings, book series, etc.)}
\details{Last update: March 2011. The data file is based on the official and publicly available
(no permission needed-information from Elsevier) Scopus list of covered titles,
see \url{http://www.info.sciverse.com/documents/files/scopus-training/resourcelibrary/xls/title_list.xls}.

This data frame consists of 30017 records.
It has the following columns.
\tabular{ll}{
\code{Record} \tab Unique source identifier in \emph{SciVerse Scopus} (character string). \cr
\code{Title} \tab Title of the source. \cr
\code{ISSN_Print} \tab Print ISSN (8 characters). \cr
\code{ISSN_E} \tab E-ISSN (8 characters). \cr
\code{Status} \tab Status of the source, either \code{Active} or \code{Inactive}. \cr
\code{SJR_2007} \tab SCImago Journal Rank for 2007. \cr
\code{SNIP_2007} \tab Source Normalized Impact per Paper for 2007. \cr
\code{SJR_2008} \tab SCImago Journal Rank for 2008. \cr
\code{SNIP_2008} \tab Source Normalized Impact per Paper for 2008. \cr
\code{SJR_2009} \tab SCImago Journal Rank for 2009. \cr
\code{SNIP_2009} \tab Source Normalized Impact per Paper for 2009. \cr
\code{OpenAccess} \tab Type of Open Access, see below. \cr
\code{Type} \tab Type of the source, see below. \cr
\code{Country} \tab Country of origin. \cr
\code{ASJC} \tab A list of semicolon-separated ASJC classification codes, see \code{\link{Scopus_ASJC}}. \cr
}

\code{OpenAccess} is one of \code{DOAJ}, \code{Not OA} (not Open Access source),
\code{OA but not registered}, \code{OA registered}.

\code{Type} is one of \code{Book Series}, \code{Conference Proceedings}, \code{Journal}, \code{Trade Journal}

The \code{data.frame} is sorted by \code{ISSN_Print} and \code{Status} (secondary criterion; \code{Active} sources first).}
\docType{data}
\seealso{\code{\link{Scopus_ASJC}}, \code{\link{Scopus_ReadCSV}}, \code{\link{Scopus_ImportSources}}}
\author{Marek Gagolewski \email{gagolews@ibspan.waw.pl}}
\references{\url{http://www.info.sciverse.com/scopus/scopus-in-detail/facts/}\cr
\url{http://info.scopus.com/journalmetrics/sjr.html}\cr
\url{http://info.scopus.com/journalmetrics/snip.html}\cr}
\keyword{Scopus,}
\keyword{ASJC,}
\keyword{journal,}
\keyword{conference,}
\keyword{proceedings}
