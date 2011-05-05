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







#' /internal/
#' Imports data from Scopus_ASJC to Biblio_Categories
.Scopus_ImportSources_Categories <- function(conn, verbose)
{
	if (verbose) cat("Importing Scopus ASJC codes... ");

	
	## ----- prepare queries ----------------------------------------------
	queries <- sprintf("INSERT INTO Biblio_Categories
	          ('IdCategory', 'IdCategoryGroup', 'Description')
		VALUES (%g,           %g,                '%s');",
			as.integer(Scopus_ASJC$ASJC),
			as.integer(Scopus_ASJC$ASJC_Group),
			sqlEscapeTrim(Scopus_ASJC$Description)
		);
	## --------------------------------------------------------------------


	dbBeginTransaction(conn);
	for (i in 1:nrow(Scopus_ASJC)) dbExecQuery(conn, queries[i], TRUE);
	dbCommit(conn);

	if (verbose) cat(sprintf("OK, %g records added.\n", nrow(Scopus_ASJC)));
}




#' /internal/
#' Imports country list form Scopus_SourceList to Biblio_Countries
#' Returns Countries converted to IdCountry for direct use in further queries.
.Scopus_ImportSources_Countries <- function(conn, verbose)
{
	if (verbose) cat("Importing country list... ");




	IdCountry <- as.factor(Scopus_SourceList$Country);
	
	lev <- levels(IdCountry);
	lev <- sqlEscapeTrim(lev);
	
	lev[nchar(lev) == 0] <- NA;
	
	existing <- which(!is.na(lev));
	n <- length(existing);
	
	
	## ----- prepare queries ------------------------
	queries <- sprintf("INSERT INTO Biblio_Countries
	          ('IdCountry', 'Name')
	   VALUES (%g,            '%s');",
		        1:n,   lev[existing]);
	
	
	## ----- exec    queries ------------------------
	dbBeginTransaction(conn);
	for (i in 1:n) dbExecQuery(conn, queries[i], TRUE);
	dbCommit(conn);
	
	if (verbose) cat(sprintf("OK, %g records added.\n", n));
	
	
	levels(IdCountry)[existing]  <- 1:n;

	return(IdCountry);
}




#' /internal/
.Scopus_ImportSources_Sources <- function(conn, IdCountry, Impact, verbose)
{
	n <- as.integer(nrow(Scopus_SourceList));
	
	if (verbose) cat("Importing Scopus source list... ");
	
	
	## ----- prepare queries ------------------------
	queries <- sprintf("INSERT OR FAIL INTO Biblio_Sources(
	      'IdSource', 'Title', 'ISSN_Print', 'ISSN_E', 'IsActive',
	      'IsOpenAccess', 'Type', 'IdCountry', 'Impact'
		)
		VALUES (%g, '%s', %s, %s, %s, %s, %s, %s, %s);",
		1:n,
		sqlEscapeTrim(Scopus_SourceList$Title),
		sqlStringOrNULL(Scopus_SourceList$ISSN_Print),
		sqlStringOrNULL(Scopus_SourceList$ISSN_E),
		as.integer(Scopus_SourceList$Status == "Active"),
		as.integer(Scopus_SourceList$OpenAccess != "Not OA"),
		sqlSwitchOrNULL(Scopus_SourceList$Type,
			CITAN:::.lbs_SourceTypesFull,
			CITAN:::.lbs_SourceTypesShort
		),
		sqlNumericOrNULL(IdCountry),
		sqlNumericOrNULL(Impact)
	);
	
	## ------ prepare ASJCs --------------------------
	
	asjcs <- strsplit(Scopus_SourceList$ASJC, "[[:space:]]*;[[:space:]]*");
	asjcs <- lapply(asjcs, function(x)
		{ x<- na.omit(as.integer(x));
		  x<- x[x>=1000 & x<=9999];
		  x;
		});
	
	stopifnot(length(asjcs) == nrow(Scopus_SourceList));
	
	
	

	if (verbose) window <- CITAN:::.gtk2.progressBar(0, n, info="Importing source list... ");


	## ----- exec    queries ------------------------
	omitted <- numeric(0);
	k <- 0;
	dbBeginTransaction(conn);
	for (i in 1:n)
	{
		tryCatch(
		{
			res <- dbSendQuery(conn, queries[i]);
			dbClearResult(res);
# 			id <- as.numeric(dbGetQuery(conn, "SELECT last_insert_rowid()")[1,1]); ## == i
			
			m <- length(asjcs[[i]]);
			if (m > 0)
			{
				queries2 <- sprintf("INSERT INTO Biblio_SourcesCategories('IdSource', 'IdCategory')
					VALUES(%g, %g);", i, asjcs[[i]]);
					
				for (j in 1:m)
					dbExecQuery(conn, queries2[j], TRUE);
					
				k <- k+m;
			} else warning(sprintf("No ASJC @ row=%g.", i));
		},
		error=function(err)
		{
# 			print(i);
# 			print(err);
			omitted <<- c(omitted, i);
		});
		
		if (verbose) CITAN:::.gtk2.progressBar(i, n, window=window);
	}
	dbCommit(conn);



	## -----  now check which rows were added and report missing values ------
	if (length(omitted) > 0)
	{
		warning(sprintf("%g records omitted due to not unique ISSNs @ rows=%s.",
			length(omitted), paste(omitted,collapse=","))
		);
	}

	if (verbose) cat(sprintf("OK, %g of %g records added; %g ASJC codes processed.\n", n-length(omitted), n, k));
}






#' Imports \emph{SciVerse Scopus} covered titles and their ASJC codes to an empty Local Bibliometric Storage (\acronym{LBS}).
#'
#' This routine should be called prior to importing any document information
#' to the LBS with the function \code{\link{lbsImportDocuments}}.
#'
#' If multiple sources with the same ISSN (either Print-ISSN or E-ISSN)
#' are found in \code{\link{Scopus_SourceList}}
#' then only the first matching record is added to the table \code{Biblio_Sources}.
#' Additionally, a warning is generated.
#'
#' Note that adding all the sources takes some time.
#'
#' Only basic ASJC and \emph{SciVerse Scopus} source information
#' retrieved from \code{\link{Scopus_ASJC}} (stored in the table \code{Biblio_Categories})
#' and \code{\link{Scopus_SourceList}} (stored in tables \code{Biblio_Countries} and \code{Biblio_Sources})
#' will be added to the LBS.
#'
#'
#' @title Import SciVerse Scopus coverage information and ASJC codes to a Local Bibliometric Storage
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param impactColumn single character value determining column name in \code{\link{Scopus_SourceList}} with values of some source impact measurements to be imported, e.g. \code{"SJR_2009"}, or \code{NULL} if no such data should be saved.
#' @param verbose logical; \code{TRUE} to inform about the progress of the process.
#' @return \code{TRUE} on success.
#' @export
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' lbsCreate(conn);
#' Scopus_ImportSources(conn);
#' ## ...
#' dbDisconnect(conn);}
#' @seealso \code{\link{Scopus_ASJC}}, \code{\link{Scopus_SourceList}}, \code{\link{Scopus_ReadCSV}}, \code{\link{lbsConnect}}, \code{\link{lbsCreate}}
Scopus_ImportSources <- function(conn, impactColumn=NULL, verbose=T)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection
	
	
	## ---- check if 4 tables used here are empty -----------------------------
	res <- dbGetQuery(conn, "SELECT COUNT(*) AS Count FROM Biblio_Categories");
	if (res[1,1] != 0) stop("table 'Biblio_Categories' is not empty.");
	
	res <- dbGetQuery(conn, "SELECT COUNT(*) AS Count FROM Biblio_Sources");
	if (res[1,1] != 0) stop("table 'Biblio_Sources' is not empty.");
	
	res <- dbGetQuery(conn, "SELECT COUNT(*) AS Count FROM Biblio_Countries");
	if (res[1,1] != 0) stop("table 'Biblio_Countries' is not empty.");

	res <- dbGetQuery(conn, "SELECT COUNT(*) AS Count FROM Biblio_SourcesCategories");
	if (res[1,1] != 0) stop("table 'Biblio_SourcesCategories' is not empty.");
	## ------------------------------------------------------------------------
	

	## ------  check impactColumn   ------------------------------------------
	if (is.null(impactColumn)) {
		Impact <- NA;
	} else {
		Impact <- Scopus_SourceList[impactColumn]; # stops if undefined
		if (class(Impact) != "numeric") stop("given 'impactColumn' is not numeric.");
	}
	## ------------------------------------------------------------------------


	.Scopus_ImportSources_Categories(conn, verbose)

	IdCountry <- .Scopus_ImportSources_Countries(conn, verbose)

	.Scopus_ImportSources_Sources(conn, IdCountry, Impact, verbose)


	## --------- vacuum -----------------------------------------------------
	dbExecQuery(conn, "VACUUM", FALSE);
	## ----------------------------------------------------------------------

	return(TRUE);
}
