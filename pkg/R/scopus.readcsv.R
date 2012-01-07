## This file is part of the CITAN library.
##
## Copyright 2011-2012 Marek Gagolewski
##
##
## CITAN is free software: you can redistribute it and/or modify
## it under the terms of the GNU Lesser General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## CITAN is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
## GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with CITAN. If not, see <http://www.gnu.org/licenses/>.


#' @include biblio.internal.R
NA




#' Reads bibliography entries from a UTF-8 encoded CSV file created e.g.
#' with \emph{SciVerse Scopus} (e.g. Complete format or Citations only).
#'
#' The function \code{\link{read.csv}}
#' is used to read the bibliometric database. However, you may freely modify its behavior
#' by passing further arguments (\code{...}), see the manual page of \code{\link{read.table}}
#' for details.
#'
#' The CSV file should consist of at least the following columns.
#' \enumerate{
#' \item \code{Authors}: Author name(s) (surname first; multiple names are comma-separated,
#' e.g. \dQuote{Kovalsky John, Smith B. W.}),
#' \item \code{Title}: Document title,
#' \item \code{Year}: Year of publication,
#' \item \code{Source.title}: Source title, e.g. journal name,
#' \item \code{Volume}: Volume number,
#' \item \code{Issue}: Issue number,
#' \item \code{Page.start}: Start page number,
#' \item \code{Page.end}: End page number,
#' \item \code{Cited.by}: Number of citations received,
#' \item \code{Link}: String containing unique document identifier of the form ...id=\emph{\strong{UNIQUE_ID}}&...
#' \item \code{Document.Type}: Document type, one of: \dQuote{Article}, \dQuote{Article in Press},
#'        \dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial},
#'        \dQuote{Erratum}, \dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
#'        \dQuote{Review}, \dQuote{Short Survey}, or \code{NA}
#'        (other categories are interpreted as \code{NA}),
#' \item \code{Source}: Data source identifier, must be the same as the value
#'        of \code{dbIdentifier} parameter.
#' }
#'
#' Such a CSV file may be generated e.g. with \emph{SciVerse Scopus}
#' (Export format=\emph{comma separated file, .csv (e.g. Excel)},
#' Output=\emph{Complete format} or \emph{Citations only}). Note that the exported CSV file
#' sometimes needs to be corrected (wrong page numbers, single
#' double quotes in character strings instead of two-double quotes etc.).
#' We suggest to make the corrections in a \dQuote{Notepad}-like application
#' (in plain text).
#' The function tries to indicate line numbers causing
#' potential problems.
#'
#' @title Import bibliography entries from a CSV file.
#' @param filename the name of the file which the data are to be read from, see \code{\link{read.csv}}.
#' @param stopOnErrors logical; \code{TRUE} to stop on all potential parse errors or just warn otherwise.
#' @param dbIdentifier single character value; database identifier, helps detect parse errors, see above.
#' @param ... further arguments to be passed to \code{read.csv}.
#' @return A data frame (\code{data.frame}) containing the following 11 columns:
#' \tabular{ll}{
#' \code{Authors} \tab	Author(s) name(s), comma-separated, surnames first.\cr
#' \code{Title} \tab	Document title.\cr
#' \code{Year} \tab	Year of publication.\cr
#' \code{UniqueId} \tab	Unique document identifier.\cr
#' \code{SourceTitle} \tab	Title of the source containing the document.\cr
#' \code{Volume} \tab	Volume.\cr
#' \code{Issue} \tab	Issue.\cr
#' \code{PageStart} \tab	Start page; numeric.\cr
#' \code{PageEnd} \tab	End page; numeric.\cr
#' \code{Citations} \tab	Number of citations; numeric.\cr
#' \code{DocumentType} \tab	Type of the document; see above.\cr
#' }
#' Such an object may be imported into a local bibliometric storage via \code{\link{lbsImportDocuments}}.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");
#' lbsImportDocuments(conn, data, "Poland_MATH");
#' ## ...
#' dbDisconnect(conn);}
#' @seealso \code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}},
#' \code{\link{lbsConnect}},
#' \code{\link{Scopus_ImportSources}},\cr
#' \code{\link{read.table}}, \code{\link{lbsImportDocuments}}
#' @export
Scopus_ReadCSV <- function(filename, stopOnErrors=TRUE, dbIdentifier='Scopus', ...)
{
   data <- read.csv(filename, header = T, encoding="UTF-8", fileEncoding="UTF-8", ...);


   if (is.null(data$Source)) stop("Column not found: `Source'.");
   if (is.null(data$Authors)) stop("Column not found: `Authors'.");
   if (is.null(data$Title)) stop("Column not found: `Title'.");
   if (is.null(data$Year)) stop("Column not found: `Year'.");
   if (is.null(data$Source.title)) stop("Column not found: `Source.title'.");
   if (is.null(data$Volume)) stop("Column not found: `Volume'.");
   if (is.null(data$Issue)) stop("Column not found: `Issue'.");
   if (is.null(data$Page.start)) stop("Column not found: `Page.start'.");
   if (is.null(data$Page.end)) stop("Column not found: `Page.end'.");
   if (is.null(data$Cited.by)) stop("Column not found: `Cited.by'.");
   if (is.null(data$Link)) stop("Column not found: `Link'.");
   if (is.null(data$Document.Type)) stop("Column not found: `Document.Type'.");

   data$UniqueId <- gsub("^.*\\id=|\\&.*$", "", data$Link);
   data$UniqueId[data$UniqueId == ""] <- NA;


   naUniqueId <- which(is.na(data$UniqueId));
   if (length(naUniqueId) > 0)
   {
      msg <- (sprintf("some documents do not have unique identifiers. Check line %s (or its neighborhood). \
   Perhaps somethings is wrong with the end page (check for ', ' nearby).",
         naUniqueId[1]+1));

      if (stopOnErrors) stop(msg) else warning(msg);
   }

   checkUniqueId <- unique(data$UniqueId, incomparables=NA);
   if (length(checkUniqueId) != nrow(data))
   {
      msg <- (sprintf("non-unique document identifiers at rows: %s.",
         paste((1:nrow(data))[-checkUniqueId], collapse=", ")));

      if (stopOnErrors) stop(msg) else warning(msg);
   }


   data$Cited.by[!is.na(gsub("^([[:digit:]]+)$", NA, data$Cited.by))] <- NA;
   data$Cited.by <- as.numeric(data$Cited.by);
   checkCitations <- which(data$Cited.by < 0 | data$Cited.by>100000);
   if (length(checkCitations) > 0)
   {
      msg <- (sprintf("something is wrong with citation counts at rows: %s.",
         paste((1:nrow(data))[-checkCitations], collapse=", ")));

      if (stopOnErrors) stop(msg) else warning(msg);
   }



   if (any(data$Source != dbIdentifier))
   {
      msg <- (sprintf("source database does not match 'dbIdentifier'. This may possibly indicate a parse error. Check records: %s.",
         paste(which(data$Source != dbIdentifier), collapse=", ")));

      if (stopOnErrors) stop(msg) else warning(msg);
   }


   data$Page.start[!is.na(gsub("^([[:digit:]]+)$", NA, data$Page.start))] <- NA;
   data$Page.end  [!is.na(gsub("^([[:digit:]]+)$", NA, data$Page.end  ))] <- NA;

   data$Page.start <- as.numeric(data$Page.start);
   data$Page.end   <- as.numeric(data$Page.end);

   checkPages <- which((data$Page.start<0) | (data$Page.end<data$Page.start) | (data$Page.end-data$Page.start>10000));
   if (length(checkPages) > 0)
   {
      msg <- (sprintf("some documents seem to have incorrect page numbers. Check line %s (or its neighborhood).",
         checkPages[1]+1));

      if (stopOnErrors) stop(msg) else warning(msg);
   }



   data <- data.frame(Authors=data$Authors,
                      Title=data$Title,
                      Year=data$Year,
                      UniqueId=data$UniqueId,
                      SourceTitle=data$Source.title,
                      Volume=data$Volume,
                      Issue=data$Issue,
                      PageStart=data$Page.start,
                      PageEnd=data$Page.end,
                      Citations=data$Cited.by,
                      DocumentType=data$Document.Type);

   attr(data, "filename") <- filename;
   return(data);
}

