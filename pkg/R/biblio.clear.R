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



#' Tidies up a Local Bibliometric Storage
#' by removing all authors with no documents and executing the \code{VACUUM}
#' SQL command.
#'
#' @title Tidy a Local Bibliometric Storage
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @return \code{TRUE} on success.
#' @seealso \code{\link{lbsConnect}}, \code{\link{lbsCreate}},
#' \code{\link{Scopus_ImportSources}},
#' \code{\link{lbsDeleteAllAuthorsDocuments}},
#' \code{\link{dbCommit}}, \code{\link{dbRollback}}
#' @export
lbsTidy <- function(conn)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection
	
	## REMOVE ALL AUTHORS WITHOUT DOCUMENTS
	dbExecQuery(conn, "DELETE FROM Biblio_Authors WHERE IdAuthor IN (
		SELECT Biblio_Authors.IdAuthor
		FROM Biblio_Authors
		LEFT JOIN Biblio_AuthorsDocuments ON Biblio_Authors.IdAuthor=Biblio_AuthorsDocuments.IdAuthor
		WHERE IdDocument IS NULL
	)");
	chg <- dbGetQuery(conn, "SELECT changes()")[1,1];
	cat(sprintf("%g authors with no documents deleted.\n", chg));
	
	## VACUUM
	dbExecQuery(conn, "VACUUM", FALSE);
	
	return(TRUE);
}


#' Clears a Local Bibliometric Storage by dropping all tables
#' named \code{Biblio_*} and all views named \code{ViewBiblio_*}.
#'
#' For safety reasons, an SQL transaction  opened at the beginning of the
#' removal process is not committed (closed) automatically.
#' You should do it on your own (or rollback it), see Examples below.
#'
#' @title Clear a Local Bibliometric Storage
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param verbose logical; \code{TRUE} to inform about the progress of database contents' removal.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' lbsClear(conn);
#' dbCommit(conn);
#' lbsCreate(conn);
#' Scopus_ImportSources(conn);
#' ## ...
#' dbDisconnect(conn);}
#' @return \code{TRUE} on success.
#' @seealso \code{\link{lbsConnect}}, \code{\link{lbsCreate}},
#' \code{\link{Scopus_ImportSources}}, \code{\link{lbsDeleteAllAuthorsDocuments}}
#' \code{\link{dbCommit}}, \code{\link{dbRollback}}
#' @export
lbsClear <- function(conn, verbose=TRUE)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection

	dbBeginTransaction(conn);

	objects <- dbListTables(conn);
	tables <- objects[substr(objects,1,7) == "Biblio_"];
	views  <- objects[substr(objects,1,11) == "ViewBiblio_"];


	if (length(tables) == 0 && length(views) == 0)
	{
		warning("database is already empty.");
		return(TRUE);
	}
	


	if (length(tables) != 0)
		for (i in 1:length(tables))
		{
			if (verbose) cat(sprintf("Dropping table '%s'... ", tables[i]));
			dbExecQuery(conn, sprintf("DROP TABLE %s;", tables[i]), TRUE);
			if (verbose) cat("DONE.\n");
		}


	if (length(views) != 0)
		for (i in 1:length(views))
		{
			if (verbose) cat(sprintf("Dropping view '%s'... ", views[i]));
			dbExecQuery(conn, sprintf("DROP VIEW %s;", views[i]), TRUE);
			if (verbose) cat("DONE.\n");
		}


	warning("Transaction has not been committed yet. Do-it-yourself with dbCommit(...).");

	return(TRUE);
}





#' Deletes are author, document and survey information from a Local Bibliometric
#' Storage.
#'
#' For safety reasons, an SQL transaction opened at the beginning of the
#' removal process is not committed (closed) automatically.
#' You should do it on your own (or rollback it), see Examples below.
#'
#' @title Delete all authors, documents and surveys from a Local Bibliometric Storage
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param verbose logical; \code{TRUE} to print out the progress of database contents' removal.
#' @return \code{TRUE} on success.
#' @seealso \code{\link{lbsClear}}, \code{\link{dbCommit}}, \code{\link{dbRollback}}
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' lbsDeleteAllAuthorsDocuments(conn);
#' dbCommit(conn);
#' ## ...
#' dbDisconnect(conn);}
#' @export
lbsDeleteAllAuthorsDocuments <- function(conn, verbose=TRUE)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection
	

	if (verbose) cat(sprintf("Deleting all author and document information... "));


	dbBeginTransaction(conn);
	
	dbExecQuery(conn, "DELETE FROM Biblio_DocumentsSurveys", TRUE);
	dbExecQuery(conn, "DELETE FROM Biblio_AuthorsDocuments", TRUE);
	dbExecQuery(conn, "DELETE FROM Biblio_Surveys", TRUE);
	dbExecQuery(conn, "DELETE FROM Biblio_Authors", TRUE);
	dbExecQuery(conn, "DELETE FROM Biblio_Documents", TRUE);

	if (verbose) cat(sprintf("DONE.\n"));

	warning("Transaction has not been committed yet. Do-it-yourself with dbCommit(...).");

	return(TRUE);
}
