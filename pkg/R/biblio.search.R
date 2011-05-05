## This file is part of the CITAN library.
##
## Copyright 2011 Marek Gagolewski <gagolews@ibspan.waw.pl>
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


#' Finds authors by name.
#'
#' \code{names.like} is a set of search patterns in an SQL \code{LIKE} format,
#' i.e. an underscore \code{_} matches a single character and a percent sign
#' \code{\%} matches any set of characters. The search is case-insensitive. 
#' 
#' @title Find authors meeting given criteria
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param names.like a character vector of SQL-LIKE patterns to match authors' names.
#' @return
#' Integer vector of authors' identifiers which match at least one of given 
#' SQL-LIKE patterns.
#' @examples
#' \dontrun{
#' conn <- dbBiblioConnect("Bibliometrics.db");
#' ## ...
#' id <- lbsSearchAuthors(conn, c("Smith\%", "Knuth D.E.", "V_n \%"));
#' lbsGetInfoAuthors(conn, id);
#' ## ...}
#' @seealso \code{\link{lbsGetInfoAuthors}},
#' \code{\link{lbsSearchDocuments}},
#' \code{\link{lbsGetInfoDocuments}},\cr
#' \code{\link{lbsFindDuplicateAuthors}}
#' @export
lbsSearchAuthors <- function(conn, names.like)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection


	if (is.null(names.like) || class(names.like) != "character")
		stop("'names.like' must be a nonempty character vector.");
		
		
	query <- "SELECT IdAuthor FROM Biblio_Authors WHERE 0";
	for (i in 1:length(names.like))
		query <- paste(query, sprintf("(Name LIKE '%s')", sqlEscapeTrim(names.like[i])), sep=" OR ");

	return(dbGetQuery(conn, query)[,1]);
}



#' Searches for documents meeting given criteria (e.g. document titles,
#' documents' authors identifiers, number of citations, number of pages, publication years
#' or document types).
#'
#' \code{titles.like} is a set of search patterns in an SQL \code{LIKE} format,
#' i.e. an underscore \code{_} matches a single character and a percent sign
#' \code{\%} matches any set of characters. The search is case-insensitive. 
#'
#' The expressions passed as
#' parameters \code{citations.expr}, \code{pages.expr}, \code{year.expr}
#' must be acceptable by SQL WHERE clause in the form
#' \code{WHERE field <expression>}, see Examples below.
#'
#' @title Find documents meeting given criteria
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param titles.like a character vector of SQL-LIKE patterns to match documents' titles or \code{NULL}.
#' @param idAuthors a numeric or integer vector with author identifiers (see column \code{IdAuthor} in the table \code{Biblio_Authors}) or \code{NULL}.
#' @param citations.expr an expression determining the desired number of citations or \code{NULL}, see Examples below.
#' @param pages.expr an expression determining the desired number of pages or \code{NULL}, see Examples below.
#' @param year.expr an expression determining the desired publication year or \code{NULL}, see Examples below.
#' @param documentTypes character vector or \code{NULL}; specifies document types to restrict to;
#'    a combination of \code{Article}, \code{Article in Press}, \code{Book}, \code{Conference Paper},
#'    \code{Editorial}, \code{Erratum}, \code{Letter}, \code{Note}, \code{Report}, \code{Review},
#'    \code{Short Survey}. \code{NULL} means no such restriction.
#' @param surveyDescription single character string or \code{NULL}; survey description to restrict to or \code{NULL}.
#' @return
#' Integer vector of documents' identifiers matching given criteria.
#' @examples
#' \dontrun{
#' conn <- dbBiblioConnect("Bibliometrics.db");
#' ## ...
#' idd <- lbsSearchDocuments(conn, pages.expr=">= 400",
#'    year.expr="BETWEEN 1970 AND 1972");
#' lbsGetInfoDocuments(conn, idd);
#' ## ...}
#' @seealso \code{\link{lbsGetInfoAuthors}},
#' \code{\link{lbsSearchAuthors}},
#' \code{\link{lbsGetInfoDocuments}},\cr
#' \code{\link{lbsFindDuplicateTitles}}
#' @export
lbsSearchDocuments <- function(conn, titles.like=NULL, idAuthors=NULL,
	citations.expr=NULL,
	pages.expr=NULL,
	year.expr=NULL,
	documentTypes=NULL,
	surveyDescription=NULL)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection
	
	surveyDescription  <- CITAN:::.lbs_PrepareRestriction_SurveyDescription(conn, surveyDescription);
	documentTypesShort <- CITAN:::.lbs_PrepareRestriction_DocumentTypes(conn, documentTypes);


	if (!is.null(titles.like) && class(titles.like) != "character")
		stop("'titles.like' must be a character vector.");
		
	if (!is.null(idAuthors) && class(idAuthors) != "numeric" && class(idAuthors) != "integer")
		stop("'idAuthors' must be a numeric vector.");
		
	if (!is.null(citations.expr) && (class(citations.expr) != "character" || length(citations.expr)!=1))
		stop("'citations.expr' must be a character string.");
		
	if (!is.null(pages.expr) && (class(pages.expr) != "character" || length(pages.expr)!=1))
		stop("'pages.expr' must be a character string.");
		
	if (!is.null(year.expr) && (class(year.expr) != "character" || length(year.expr)!=1))
		stop("'year.expr' must be a character string.");
		
	if (is.null(titles.like) && is.null(documentTypes) && is.null(idAuthors) && is.null(citations.expr) && is.null(pages.expr) && is.null(year.expr))
		stop("at least one of: 'titles.like', 'documentTypes', 'idAuthors', 'citations.expr', 'pages.expr', 'year.expr' must not be NULL");
		
		
	# Get subQueryWhere
	if (length(documentTypesShort)>0)
	{
		subquery0 <- sprintf("(%s)",
			paste("Type", documentTypesShort, sep="=", collapse=" OR "));
	} else subquery0 <- "1";

	if (!is.null(surveyDescription))
		subquery0 <- paste(c(subquery0, sprintf(" Description='%s'", surveyDescription)), collapse=" AND ");

		
		
	if (is.null(titles.like)) {
		subquery1 <- "1";
	} else {
		subquery1 <- "0";
		for (i in 1:length(titles.like))
			subquery1 <- paste(subquery1, sprintf("(Title LIKE '%s')", sqlEscapeTrim(titles.like[i])), sep=" OR ");
	}

	if (is.null(idAuthors)) {
		subquery2 <- "1";
	} else {
		subquery2 <- "0";
		for (i in 1:length(idAuthors))
			subquery2 <- paste(subquery2, sprintf("(IdAuthor=%g)", idAuthors[i]), sep=" OR ");
	}
	
	if (is.null(citations.expr)) {
		subquery3 <- "1";
	} else {
		 # might be dangerous, but the user surely knows what is she doing :-)
		subquery3 <- sprintf("Citations %s", sqlTrim(citations.expr));
	}
	
	if (is.null(year.expr)) {
		subquery4 <- "1";
	} else {
		 # might be dangerous, but the user surely knows what is she doing :-)
		subquery4 <- sprintf("Year %s", sqlTrim(year.expr));
	}
	
	if (is.null(pages.expr)) {
		subquery5 <- "1";
	} else {
		 # might be dangerous, but the user surely knows what is she doing :-)
		subquery5 <- sprintf("Pages %s", sqlTrim(pages.expr));
	}
		
		
	query <- sprintf("SELECT DISTINCT Biblio_Documents.IdDocument
		FROM Biblio_Documents
		JOIN Biblio_AuthorsDocuments ON Biblio_Documents.IdDocument=Biblio_AuthorsDocuments.IdDocument
		JOIN ViewBiblio_DocumentsSurveys ON Biblio_Documents.IdDocument=ViewBiblio_DocumentsSurveys.IdDocument
		WHERE (%s) AND (%s) AND (%s) AND (%s) AND (%s) AND (%s)",
		subquery0, subquery1, subquery2, subquery3, subquery4, subquery5);
		
	return(dbGetQuery(conn, query)[,1]);
}
