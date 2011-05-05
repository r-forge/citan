\name{lbsImportDocuments}
\alias{lbsImportDocuments}
\title{Import publications to a Local Bibliometric Storage.}
\usage{lbsImportDocuments(conn, data, surveyDescription="Default survey",
    originalFilename=attr(data, "filename"), excludeRows,
    updateDocumentIfExists=TRUE, warnISSN=FALSE,
    warnExactDuplicates=FALSE, verbose=TRUE)}
\description{Imports publications from a special 14-column data frame to a Local Bibliometric Storage.
Such an input may be created e.g. with \code{\link{Scopus_ReadCSV}}.}
\details{\code{data} must consist of the following 14 columns (in order). Otherwise
the process will not be executed.
\tabular{llll}{
1  \tab \code{Authors}       \tab character\tab  Author(s) name(s), comma-separated, surnames first.\cr
2  \tab \code{Title}         \tab character\tab  Document title.\cr
3  \tab \code{Year}          \tab numeric  \tab  Year of publication.\cr
4  \tab \code{SourceTitle}   \tab character\tab  Title of the source containing the document.\cr
5  \tab \code{Volume}        \tab character\tab  Volume.\cr
6  \tab \code{Issue}         \tab character\tab  Issue.\cr
7  \tab \code{ArticleNumber} \tab character\tab  Article number (identifier).\cr
8  \tab \code{PageStart}     \tab numeric  \tab  Start page; numeric.\cr
9  \tab \code{PageEnd}       \tab numeric  \tab  End page; numeric.\cr
10 \tab \code{Citations}     \tab numeric  \tab  Number of citations.\cr
11 \tab \code{UniqueId}      \tab character\tab  Unique document identifier. \cr
12 \tab \code{ISSN}          \tab character\tab  ISSN of the source.\cr
13 \tab \code{Language}      \tab factor   \tab  Language of the document.\cr
14 \tab \code{DocumentType}  \tab factor   \tab  Type of the document.\cr
}

\code{DocumentType} is one of \dQuote{Article}, \dQuote{Article in Press},
\dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial}, \dQuote{Erratum},
\dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
\dQuote{Review}, \dQuote{Short Survey}, or \code{NA} (other categories are interpreted as \code{NA}).

Note that if \code{data} contains many records (>1000),
the import process may take a few minutes.

Sources (e.g. journals) are identified by ISSNs (table \code{Biblio_Sources}).
Note that generally there is no need to concern about missing ISSNs of
conference proceedings.

Each time a function is called, a new record in the table \code{Biblio_Surveys}
is created. Such surveys may be grouped using the \code{Description}
field, see \code{\link{lbsCreate}}.}
\value{\code{TRUE} on success.}
\seealso{\code{\link{Scopus_ReadCSV}}, \code{\link{lbsConnect}}, \code{\link{lbsCreate}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{data}{14 column \code{data.frame} with bibliometric entries; see above.}
\item{surveyDescription}{description of the survey. Allows for documents grouping.}
\item{originalFilename}{original file name, \code{attr(data, "filename")} is used by default.}
\item{excludeRows}{a numeric vector with row numbers of \code{data} to exclude or \code{NULL}.}
\item{updateDocumentIfExists}{logical; if \code{TRUE}, then documents with the same \code{UniqueId} will be updated.}
\item{warnISSN}{logical; if \code{TRUE} then warnings are generated if a given ISSN in not found in the table \code{Biblio_Sources}.}
\item{warnExactDuplicates}{logical; \code{TRUE} to warn if exact duplicates are found (turned off by default).}
\item{verbose}{logical; \code{TRUE} to inform about the progress of the process.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");
lbsImportDocuments(conn, data, "Poland_MATH");
## ...
dbDisconnect(conn);}}
