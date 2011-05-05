\name{Scopus_ReadCSV}
\alias{Scopus_ReadCSV}
\title{Import bibliography entries from a 40-column CSV file.}
\usage{Scopus_ReadCSV(filename, stopOnErrors=TRUE, dbIdentifier="Scopus", ...)}
\description{Reads bibliography entries from a 40-column CSV file created e.g.
with \emph{SciVerse Scopus} (Complete format).}
\details{The function \code{\link{read.csv}}
is used to read the bibliometric database. However, you may freely modify its behavior
by passing further arguments (\code{...}), see the manual page of \code{\link{read.table}}
for details.

The CSV file should consist of exactly 40 variables.
Here are their meanings (in order of appearance):
\enumerate{
\item Author name(s) (surname first; multiple names are comma-separated,
e.g. \dQuote{Kovalsky John, Smith B. W.}),
\item Document title,
\item Year,
\item Source title,
\item Volume.
\item Issue,
\item Article number,
\item Page start,
\item Page end,
\item \emph{not used},
\item Number of citations received,
\item String containing unique document identifier of the form ...id=\emph{\strong{UNIQUE_ID}}&...
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item Source ISSN,
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item \emph{not used},
\item Language of original document,
\item \emph{not used},
\item Document type, one of: \dQuote{Article}, \dQuote{Article in Press},
\dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial},
\dQuote{Erratum}, \dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
\dQuote{Review}, \dQuote{Short Survey}, or \code{NA}
(other categories are interpreted as \code{NA}),
\item Database identifier, should be the same as the value of \code{dbIdentifier}
parameter, otherwise an exception is thrown.
}

Such a CSV file may be generated e.g. with \emph{SciVerse Scopus}
(Export format=\emph{comma separated file, .csv (e.g. Excel)},
Output=\emph{Complete format}). Note that the exported CSV file
needs some corrections in a few cases (wrong page numbers, single
double quotes in string instead of two-double quotes etc.). 
We suggest to make them in  \dQuote{Notepad}-like applications
(in plain text).
The function tries to point out the line numbers that
cause potential problems. However, sometimes a support of Spreadsheet-like programs
could be helpful.}
\value{A data frame (\code{data.frame}) containing the following 14 columns:
\tabular{ll}{
\code{Authors} \tab	Author(s) name(s), comma-separated, surnames first.\cr
\code{Title} \tab	Document title.\cr
\code{Year} \tab	Year of publication.\cr
\code{UniqueId} \tab	Unique document identifier.\cr
\code{SourceTitle} \tab	Title of the source containing the document.\cr
\code{Volume} \tab	Volume.\cr
\code{Issue} \tab	Issue.\cr
\code{ArticleNumber} \tab Article number (identifier).\cr
\code{PageStart} \tab	Start page; numeric.\cr
\code{PageEnd} \tab	End page; numeric.\cr
\code{Citations} \tab	Number of citations.\cr
\code{ISSN} \tab	ISSN of the source.\cr
\code{Language} \tab Language of the document.\cr
\code{DocumentType} \tab	Type of the document; see above.\cr
}
Such an object may be imported to a local bibliometric storage with \code{\link{lbsImportDocuments}}.}
\seealso{\code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}},
\code{\link{lbsConnect}},
\code{\link{Scopus_ImportSources}},\cr
\code{\link{read.table}}, \code{\link{lbsImportDocuments}}}
\arguments{\item{filename}{the name of the file which the data are to be read from, see \code{\link{read.csv}}.}
\item{stopOnErrors}{logical; \code{TRUE} to stop on all potential parse errors or just warn otherwise.}
\item{dbIdentifier}{single character value; database identifier, helps detect parse errors, see above.}
\item{...}{further arguments to be passed to \code{read.csv}.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");
lbsImportDocuments(conn, data, "Poland_MATH");
## ...
dbDisconnect(conn);}}
