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


#' /internal/
#' Creates a new survey
#' @return idSurvey
.lbsImportDocuments_GetSurvey <- function(conn, surveyDescription, originalFilename, verbose)
{
   query <- sprintf("INSERT INTO Biblio_Surveys('Description', 'FileName', 'Timestamp')
      VALUES(%s, %s, %s)",
      sqlStringOrNULL(surveyDescription),
      sqlStringOrNULL(originalFilename),
      sqlStringOrNULL(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
   );
   dbExecQuery(conn, query, TRUE);

   return(as.numeric(dbGetQuery(conn, "SELECT last_insert_rowid()")[1,1]));
}





#' /internal/
.lbsImportDocuments_Add_Get_idSource <- function(conn, sourceTitle, i, warnSourceTitle)
{
   if (is.na(sourceTitle)) return(NA);
   
   sourceTitle <- sqlStringOrNULL(sourceTitle);
   idSource <- dbGetQuery(conn, sprintf("SELECT IdSource, IsActive FROM Biblio_Sources
      WHERE UPPER(Title)=UPPER(%s)",
      sourceTitle
   ));

   if (nrow(idSource)==1)
   {
      return(idSource[1,1]);
   } else if (nrow(idSource)==0)
   {
      if (warnSourceTitle)
      {
         warning(sprintf("no source with sourceTitle='%s' found for record %g. Setting IdSource=NA.",
            sourceTitle, i));
      }
      return(NA);
   } else
   {
      active = which(idSource[,2]);
      if (length(active) == 1)
      {
         if (warnSourceTitle)
            warning(sprintf("more than one source with sourceTitle='%s' found for record %g. Using the only active.", sourceTitle, i));
         return(idSource[active,1]);
      } else if (length(active) > 1)
      {
         if (warnSourceTitle)
            warning(sprintf("more than one source with sourceTitle='%s' found for record %g. Using first active.", sourceTitle, i));
         return(idSource[active[1],1]);
      } else
      {
         if (warnSourceTitle)
            warning(sprintf("more than one source with sourceTitle='%s' found for record %g. Using first (no active).", sourceTitle, i));
         return(idSource[1,1]);
      }
   }
}





#' /internal/
.lbsImportDocuments_Add <- function(conn, record, idSurvey, i,
	updateDocumentIfExists, warnExactDuplicates, warnSourceTitle, verbose)
{
   idSource <- sqlNumericOrNULL(.lbsImportDocuments_Add_Get_idSource(conn, record$SourceTitle, i, warnSourceTitle));


   res <- dbGetQuery(conn, sprintf("SELECT IdDocument, IdSource, Title, BibEntry, Citations, Type
      FROM Biblio_Documents WHERE UPPER(UniqueId)=UPPER('%s');", record$UniqueId));

   if (nrow(res) != 0)
   {
      documentExists <- TRUE;
      idDocument <- res$IdDocument[1];

      if (warnExactDuplicates)
      {
         warning(sprintf("source at row=%g already exists (IdSource=%g, Title='%s', Citations=%g, Type='%s'). %s.",
            i, res$IdSource[1], res$Title[1], res$Citations[1], res$Type[1],
            ifelse(updateDocumentIfExists, "Updating", "Ignoring")));
      }

      if (updateDocumentIfExists)
      {

          # will add authors once again later (they may be different)
         dbExecQuery(conn, sprintf("DELETE FROM Biblio_AuthorsDocuments WHERE IdDocument=%g;", idDocument), TRUE);


         # Update document
         query <- sprintf("UPDATE Biblio_Documents
            SET
               IdSource=%s,
               IdLanguage=%s,
               UniqueId='%s',
               Title='%s',
               BibEntry='%s',
               Year=%s,
               Pages=%s,
               Citations=%s,
               Type=%s
            WHERE IdDocument=%s;",

            idSource,
            record$IdLanguage,
            record$UniqueId,
            record$Title,
            record$BibEntry,
            record$Year,
            record$Pages,
            record$Citations,
            record$DocumentType,
            sqlNumericOrNULL(idDocument)
         );
         dbExecQuery(conn, query, TRUE);
      }
   } else {
      documentExists <- FALSE;


      # Insert document
      query <- sprintf("INSERT OR FAIL INTO Biblio_Documents ('IdSource', 'IdLanguage',
         'UniqueId', 'Title', 'BibEntry', 'Year', 'Pages', 'Citations', 'Type')
         VALUES(%s, %s, '%s', '%s', '%s', %s, %s, %s, %s);",
         idSource,
         record$IdLanguage,
         record$UniqueId,
         record$Title,
         record$BibEntry,
         record$Year,
         record$Pages,
         record$Citations,
         record$DocumentType
      );
      dbExecQuery(conn, query, TRUE);


      idDocument <- dbGetQuery(conn, "SELECT last_insert_rowid()")[1,1];
   }



   query <- sprintf("INSERT INTO Biblio_DocumentsSurveys (IdDocument, IdSurvey) VALUES(%s, %s);",
      sqlNumericOrNULL(idDocument), sqlNumericOrNULL(idSurvey));
   dbExecQuery(conn, query, TRUE);


   if (documentExists && !updateDocumentIfExists) return(FALSE);


   # add authors
   authors <- strsplit(record$Authors, "[[:space:]]*,[[:space:]]*")[[1]];
   stopifnot(length(authors)>0);
   for (j in 1:length(authors))
   {
      stopifnot(nchar(authors[j])>0);


      # Get idAuthor (and add him/her if necessary)
      idAuthor <- dbGetQuery(conn, sprintf("SELECT IdAuthor FROM Biblio_Authors WHERE UPPER(Name)=UPPER(%s)",
         sqlStringOrNULL(authors[j])
      ));
      if (nrow(idAuthor) == 0)
      {
         dbExecQuery(conn, sprintf("INSERT INTO Biblio_Authors(Name) VALUES(%s);", sqlStringOrNULL(authors[j])), TRUE);
         idAuthor <- dbGetQuery(conn, "SELECT last_insert_rowid()")[1,1];
      } else {
         idAuthor <- idAuthor[1,1];
      }


      query <- sprintf("INSERT OR IGNORE INTO Biblio_AuthorsDocuments(IdAuthor, IdDocument)
         VALUES(%s, %s);",
         sqlNumericOrNULL(idAuthor),
         sqlNumericOrNULL(idDocument));
      dbExecQuery(conn, query, TRUE);
   }
   
   return(!documentExists)
}



#' Imports publications from a special 11-column data frame to a Local Bibliometric Storage.
#' Such an input may be created e.g. with \code{\link{Scopus_ReadCSV}}.
#'
#'
#' \code{data} must consist of the following 14 columns (in order). Otherwise
#' the process will not be executed.
#' \tabular{llll}{
#' 1  \tab \code{Authors}       \tab character\tab  Author(s) name(s), comma-separated, surnames first.\cr
#' 2  \tab \code{Title}         \tab character\tab  Document title.\cr
#' 3  \tab \code{Year}          \tab numeric  \tab  Year of publication.\cr
#' 4  \tab \code{SourceTitle}   \tab character\tab  Title of the source containing the document.\cr
#' 5  \tab \code{Volume}        \tab character\tab  Volume.\cr
#' 6  \tab \code{Issue}         \tab character\tab  Issue.\cr
#' 7  \tab \code{PageStart}     \tab numeric  \tab  Start page; numeric.\cr
#' 8  \tab \code{PageEnd}       \tab numeric  \tab  End page; numeric.\cr
#' 9  \tab \code{Citations}     \tab numeric  \tab  Number of citations; numeric.\cr
#' 10 \tab \code{UniqueId}      \tab character\tab  Unique document identifier. \cr
#' 11 \tab \code{DocumentType}  \tab factor   \tab  Type of the document.\cr
#' }
#'
#' \code{DocumentType} is one of \dQuote{Article}, \dQuote{Article in Press},
#'        \dQuote{Book}, \dQuote{Conference Paper}, \dQuote{Editorial}, \dQuote{Erratum},
#'        \dQuote{Letter}, \dQuote{Note}, \dQuote{Report},
#'        \dQuote{Review}, \dQuote{Short Survey}, or \code{NA} (other categories are interpreted as \code{NA}).
#'
#' Note that if \code{data} contains many records (>1000),
#' the import process may take a few minutes.
#'
#' Sources (e.g. journals) are identified by SourceTitle (table \code{Biblio_Sources}).
#' Note that generally there is no need to concern about missing SourceTitles of
#' conference proceedings.
#'
#' Each time a function is called, a new record in the table \code{Biblio_Surveys}
#' is created. Such surveys may be grouped using the \code{Description}
#' field, see \code{\link{lbsCreate}}.
#'
#' @title Import publications to a Local Bibliometric Storage.
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param data 11 column \code{data.frame} with bibliometric entries; see above.
#' @param surveyDescription description of the survey. Allows for documents grouping.
#' @param originalFilename original file name, \code{attr(data, "filename")} is used by default.
#' @param excludeRows a numeric vector with row numbers of \code{data} to exclude or \code{NULL}.
#' @param updateDocumentIfExists logical; if \code{TRUE}, then documents with the same \code{UniqueId} will be updated.
#' @param warnSourceTitle logical; if \code{TRUE} then warnings are generated if a given SourceTitle in not found in the table \code{Biblio_Sources}.
#' @param warnExactDuplicates logical; \code{TRUE} to warn if exact duplicates are found (turned off by default).
#' @param verbose logical; \code{TRUE} to inform about the progress of the process.
#' @return  \code{TRUE} on success.
#' @seealso \code{\link{Scopus_ReadCSV}}, \code{\link{lbsConnect}}, \code{\link{lbsCreate}}
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' data <- Scopus_ReadCSV("db_Polish_MATH/Poland_MATH_1987-1993.csv");
#' lbsImportDocuments(conn, data, "Poland_MATH");
#' ## ...
#' dbDisconnect(conn);}
#' @export
lbsImportDocuments <- function(conn, data, surveyDescription="Default survey",
   originalFilename=attr(data, "filename"),
   excludeRows=NULL,  updateDocumentIfExists=TRUE,
   warnSourceTitle=TRUE, warnExactDuplicates=FALSE, verbose=TRUE)
{
   CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection



   ## ------ check data ------------------------------------------------------

   if (class(data) != "data.frame")
      stop("'data' is not a data.frame.");

   if (ncol(data) != 11 || is.null(data$Authors) || is.null(data$Title)
      || is.null(data$Year) || is.null(data$SourceTitle) || is.null(data$Volume)
      || is.null(data$Issue) || is.null(data$PageStart) || is.null(data$PageEnd)
      || is.null(data$Citations) || is.null(data$UniqueId)
      || is.null(data$DocumentType))
      stop("incorrect format of 'data'.");

   data$Authors <- as.character(data$Authors);
   if (class(data$PageStart)!="numeric")       stop("column 'PageStart' in 'data' should be 'numeric'.");
   if (class(data$PageEnd)!="numeric")         stop("column 'PageEnd' in 'data' should be 'numeric'.");
   if (class(data$Citations)!="numeric")       stop("column 'Citations' in 'data' should be 'numeric'.");


   if (!is.null(excludeRows) && !is.numeric(excludeRows))
      stop("'excludeRows' must be numeric or NULL.");

   if (is.null(originalFilename) || is.na(originalFilename) || length(originalFilename) != 1)
      originalFilename <- "Unknown filename";

   if (is.null(surveyDescription) || is.na(surveyDescription) || length(surveyDescription) != 1)
      surveyDescription <- "Default survey";

   ## -------------------------------------------------------------------

   if (verbose) cat("Importing documents and their authors... ");


   if (!is.null(excludeRows))
      data <- data[-excludeRows,];


   ## CREATE A NEW SURVEY
   idSurvey <- .lbsImportDocuments_GetSurvey(conn, surveyDescription, originalFilename, verbose);
   stopifnot(length(idSurvey) == 1 && is.finite(idSurvey));

   ## PREPARE IdLanguage
   data$IdLanguage <- "NULL"; # data does not provide documents' language

   ## PREPARE BibEntry
   data$BibEntry <- sqlEscape(paste(
      sqlTrim(data$SourceTitle),
      sqlTrim(data$Year),
      sqlTrim(data$Volume),
      sqlTrim(data$Issue),
      sqlTrim(data$ArticleNumber),
      data$PageStart,
      data$PageEnd,
      sep=","));

   ## PREPARE other fields
   data$SourceTitle <- sqlEscapeTrim(data$SourceTitle);
   data$UniqueId <- sqlEscapeTrim(data$UniqueId);
   data$Title <- sqlEscapeTrim(data$Title);
   data$Year <- sqlNumericOrNULL(data$Year);
   data$Pages <- sqlNumericOrNULL(data$PageEnd-data$PageStart+1);
   data$Citations <- ifelse(is.finite(data$Citations), data$Citations, 0);
   data$DocumentType <- sqlSwitchOrNULL(data$DocumentType,
            CITAN:::.lbs_DocumentTypesFull,
            CITAN:::.lbs_DocumentTypesShort
         );

   ## -------------------------------------------------------------------


   k <- 0L;
   n <- as.integer(nrow(data));

   if (verbose)
      window <- CITAN:::.gtk2.progressBar(0, n,
         info=sprintf("Importing %g documents and their authors to %s/%s...",
         n, surveyDescription, originalFilename));

   dbBeginTransaction(conn);
   for (i in 1:n)
   {
      if (.lbsImportDocuments_Add(conn, data[i,], idSurvey, i,
         updateDocumentIfExists, warnExactDuplicates, warnSourceTitle, verbose))
      {
         k <- k+1L;
      }

      if (verbose) CITAN:::.gtk2.progressBar(i,n,window=window);
   }
   dbCommit(conn);


   if (verbose) cat(sprintf("OK, %g of %g records added to %s/%s.\n", k, n, surveyDescription, originalFilename));

   ## -------------------------------------------------------------------

   return(TRUE);
}
