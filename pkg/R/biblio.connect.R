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



#' Connects to a Local Bibliometric Storage handled by the SQLite engine
#' (see \pkg{RSQLite} package documentation).
#'
#' Do not forget to close the connection (represented by the object returned)
#' with \code{\link{dbDisconnect}} after use.
#' Note that you may freely access the database using
#' functions from the \pkg{DBI} package called on the
#' returned connection object.
#'
#' @title Connect to a Local Bibliometric Storage
#' @param dbfilename name of the file storing the SQLite database.
#' @return An object of type \code{SQLiteConnection}.
#' It is used to direct commands to the SQLite engine.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' dbDisconnect(conn);}
#' @seealso \code{\link{lbsCreate}}, \code{\link{dbDisconnect}}
#' @export
lbsConnect <- function(dbfilename)
{
	if (length(dbfilename)!=1 || !is.character(dbfilename))
		stop("incorrect 'dbfilename' given");

	drv <- dbDriver("SQLite");
	dbConnect(drv, dbname = dbfilename);
}





