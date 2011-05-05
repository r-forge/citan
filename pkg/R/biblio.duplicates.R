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
.lbsFindDuplicateTitles_getDups <- function(conn, surveyDescription, ignoreTitles.like, aggressiveness)
{
	surveyDescription  <- CITAN:::.lbs_PrepareRestriction_SurveyDescription(conn, surveyDescription);

	cat("Looking for documents with duplicate titles... "); 
	dups <- list();
	res <- NULL;
	
	if (!is.null(ignoreTitles.like))
		ignoreTitles.like <- toupper(ignoreTitles.like);
	
	if (aggressiveness == 0)
	{
		res <- dbGetQuery(conn, sprintf(
			"SELECT DupRes.IdDocument, DupRes.Title, DupRes.Type, COUNT(IdAuthor) AS AuthorCount
			FROM
			(
				SELECT DISTINCT Biblio_Documents.IdDocument, Title, Type, Citations
				FROM Biblio_Documents
				JOIN ViewBiblio_DocumentsSurveys ON Biblio_Documents.IdDocument=ViewBiblio_DocumentsSurveys.IdDocument
				WHERE %s AND %s AND Title IN (
					SELECT Title FROM (
						SELECT Title, COUNT(IdDocument) AS cnt FROM Biblio_Documents
						GROUP BY Title
					) WHERE cnt > 1
				)
			) AS DupRes
			JOIN Biblio_AuthorsDocuments ON DupRes.IdDocument=Biblio_AuthorsDocuments.IdDocument
			GROUP BY DupRes.IdDocument
			ORDER BY Title ASC, Citations DESC;",
			ifelse(is.null(surveyDescription), "1", sprintf(" Description='%s'", surveyDescription)),
			ifelse(is.null(ignoreTitles.like), "1", paste("NOT Title LIKE", sprintf("'%s'", ignoreTitles.like), collapse=" AND ", sep=" "))
		));
		
		n <- nrow(res);
		if (n <= 1) return(dups);
		

		window <- CITAN:::.gtk2.progressBar(0, n, info="Looking for documents with duplicate titles...");
		
		res$Title <- toupper(res$Title);
		
		k <- 0;
		i <- 1;
		while (i <= n)
		{
			j <- i+1;
			while (j <= n && (res$Title[i] == res$Title[j]))
				j <- j+1;
				
			k <- k+1;
			dups[[k]] <- i:(j-1);
			
			CITAN:::.gtk2.progressBar(j-1,n,each=1,window=window);
			i <- j;
		}
	} else if (aggressiveness >= 1)
	{
		query <- sprintf("
			SELECT IdDocument, Title, Type, Citations, COUNT(IdAuthor) AS AuthorCount
			FROM
			(
				SELECT DISTINCT Biblio_Documents.IdDocument, Title, Type, Citations, IdAuthor
				FROM Biblio_Documents
				JOIN ViewBiblio_DocumentsSurveys  ON Biblio_Documents.IdDocument=ViewBiblio_DocumentsSurveys.IdDocument
				LEFT JOIN Biblio_AuthorsDocuments ON Biblio_Documents.IdDocument=Biblio_AuthorsDocuments.IdDocument
				WHERE %s AND %s
			)
			GROUP BY IdDocument
			ORDER BY Title ASC, Citations DESC;",
			ifelse(is.null(ignoreTitles.like), "1", paste("NOT Title LIKE", sprintf("'%s'", ignoreTitles.like), collapse=" AND ", sep=" ")),
			ifelse(is.null(surveyDescription), "1", sprintf(" Description='%s'", surveyDescription))
		);
	
		res <- dbGetQuery(conn, query);
		
		n <- nrow(res);

		if (n <= 1) return(dups);
		
		res$Title <- toupper(res$Title);
		
		if (aggressiveness == 1)
		{
			res$TitleComp <- (gsub("[^[:alpha:]]*", "", res$Title));
		} else
		{
			window <- CITAN:::.gtk2.progressBar(i, n, info="Preparing data...");
			res$TitleComp <- character(n);
			mtch <- gregexpr("[[:alpha:]]+", res$Title);
			for (i in 1:n)
			{
				m <- mtch[[i]];
				idx <- which(attr(m, "match.length") >= aggressiveness);
				if (length(idx) == 0)
				{
					res$TitleComp[i] <- (gsub("[^[:alpha:]]*", "", res$Title[i]));
				} else
				{
					res$TitleComp[i] <- toupper(paste(
						sapply(idx, function(x) {
							substr(res$Title[i], m[x], m[x]+attr(m, "match.length")[x]-1)
						}),
						sep="", collapse=""));
				}
				
				CITAN:::.gtk2.progressBar(i, n, window=window);
			}
		}
		
		
		res <- res[order(res$TitleComp),];

		
		window <- CITAN:::.gtk2.progressBar(0, n, info="Looking for documents with duplicate titles...");
		
		k <- 0;
		i <- 1;
		while (i <= n)
		{
			j <- i+1;
			while (j <= n && (
				   substr(res$TitleComp[i],1,min(nchar(res$TitleComp[i]),nchar(res$TitleComp[j]))) ==
				   substr(res$TitleComp[j],1,min(nchar(res$TitleComp[i]),nchar(res$TitleComp[j])))))
				j <- j+1;
			
			if (j-1 > i)
			{
				k <- k+1;
				dups[[k]] <- i:(j-1);
			}
			
			CITAN:::.gtk2.progressBar(j-1,n,each=1,window=window);
			i <- j;
		}
	}
	
	cat("DONE.\n");
	
	
	k <- length(dups);
	if (k == 0) return(dups);
	
	
	# Now, re-order dups according to some heuristic utility function
	# Assumptions: article-in-press++
	# letter--, editorial--, erratum--, note--
	# similar titles and number of authors+
	# similar titles but not number of authors-
	# many results---
	# take only two fist elements into account (they are sorted by title)
	utility <- rep(0, k);

	for (i in 1:k)
	{
		check <- dups[[i]];
		stopifnot(length(check)>=2);
		
		if (gsub("[^[:alpha:]]*", "", res$Title[check[1]]) == gsub("[^[:alpha:]]*", "", res$Title[check[2]]))
			utility[i] <- utility[i] + 1.0;
			
		utility[i] <- utility[i] + ifelse((res$AuthorCount[check[1]] == res$AuthorCount[check[2]]), 1.5, -2.3);
		if (all(!is.na(res$Type[check[1:2]])))
		{
			if (any(res$Type[check[1:2]]=="le") || 
					any(res$Type[check[1:2]]=="no") ||
					any(res$Type[check[1:2]]=="ed") ||
					any(res$Type[check[1:2]]=="er"))
				utility[i] <- utility[i]-1.3;
				
			if (any(res$Type[check[1:2]]=="ip")) utility[i] <- utility[i]+2.5;
		}

		utility[i] <- utility[i]-sqrt(length(check))+sqrt(2);
	}
	
	
	ord <- order(utility, decreasing=TRUE);
	dups <- dups[ord];
	for (i in 1:k)
	{
		dups[[i]] <- res$IdDocument[dups[[i]]];
	}
	
	
	return(dups);
}


#' This function suggests the user some groups of documents that possibly
#' should be merged.
#' It is based on documents' titles similarity comparisons.
#' It uses a heuristic algorithm, which behavior
#' is controlled by the \code{aggressiveness} parameter.
#'
#' The search results are presented in a convenient-to-use graphical dialog box.
#' The function tries to order the groups of documents according
#' to their relevance (**EXPERIMENTAL** algorithm).
#' Note that the calculation may take a few minutes!
#'
#' \code{ignoreTitles.like} is a set of search patterns in an SQL \code{LIKE} format,
#' i.e. an underscore \code{_} matches a single character and a percent sign
#' \code{\%} matches any set of characters. The search is case-insensitive. 
#' 
#' @title Suggest documents to be merged (**EXPERIMENTAL**)
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param surveyDescription single character string or \code{NULL}; survey description to restrict to or \code{NULL}.
#' @param ignoreTitles.like a character vector of SQL-LIKE patterns to match documents' titles to be ignored or \code{NULL}.
#' @param aggressiveness nonnegative integer; \code{0} for showing only exact matches;
#' the higher the value, the more documents will be proposed.
#' @return
#' A numeric vector of user-selected documents' identifiers to be removed.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' listdoc <- lbsFindDuplicateTitles(conn,
#'    ignoreTitles.like=c("\%In this issue\%", "\%Editorial", "\%Introduction",
#'    "Letter to \%", "\%Preface"),
#'    aggressiveness=2);
#' lbsDeleteDocuments(conn, listdoc);
#' dbCommit(conn);
#' ## ...}
#' @seealso \code{\link{lbsDeleteDocuments}}, \code{\link{lbsFindDuplicateAuthors}}, \code{\link{lbsGetInfoDocuments}}
#' @export
lbsFindDuplicateTitles <- function(conn,
	surveyDescription=NULL,
	ignoreTitles.like=NULL,
	aggressiveness=1)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection
	
	if (!is.numeric(aggressiveness) || length(aggressiveness)!=1 || aggressiveness<0)
		stop("incorrect 'aggressiveness'.");
		
	if (!is.null(ignoreTitles.like) && !is.character(ignoreTitles.like))
		stop("incorrect 'ignoreTitles.like'.");
		
	
	dups <- .lbsFindDuplicateTitles_getDups(conn, surveyDescription, ignoreTitles.like, aggressiveness);
	n <- length(dups);
	
	if(n == 0) return(NULL);
	
	removed <- integer(0);
	for (i in 1:n)
	{
		ret <- CITAN:::.gtk2.selectDocuments(conn, dups[[i]], sprintf("Select documents to remove (stage %g of %g)", i, n), remove=TRUE);
		
		if (is.null(ret))
			return(removed);

		removed <- c(removed, ret);
	}
	
	return(removed);
}




#' /internal/
.lbsFindDuplicateAuthors_split2Words <- function(what, ignoreWords, minWordLength)
{
	n <- length(what);

	window <- CITAN:::.gtk2.progressBar(0, n, info="Preprocessing data...");
	
	mtch <- gregexpr("[^[:space:]]+", what);
	words <- list();
	length(words) <- n;
		
	for (i in 1:n)
	{
		match_i <- mtch[[i]];
		m <- length(match_i);
		words[[i]] <- character(0);
		if (m > 0)
		{
			for (j in 1:m)
			{
				wrd <- substr(what[i], match_i[j], match_i[j]+attr(match_i,"match.length")[j]-1);
				
				if (any(wrd == ignoreWords)) next;
				if (substr(wrd, nchar(wrd), nchar(wrd)) == ".") next; # last letter == "."
				if (nchar(wrd) < minWordLength) next;
				
				words[[i]] <- c(words[[i]], wrd);
			}
		}
		CITAN:::.gtk2.progressBar(i, n, window=window);
	}
	
	return(words);
}



#' /internal/
.lbsFindDuplicateAuthors_getDupsGraph <- function(conn, names.like, ignoreWords, minWordLength, aggressiveness, orderResultsBy)
{
	query <- sprintf(
		"SELECT Biblio_Authors.IdAuthor AS IdAuthor, Name,
			COUNT(IdDocument) As Documents,
			SUM(Citations) As Citations
		FROM Biblio_Authors
		JOIN (
			SELECT Biblio_AuthorsDocuments.IdAuthor, Biblio_AuthorsDocuments.IdDocument, Biblio_Documents.Citations
			FROM Biblio_AuthorsDocuments
			JOIN Biblio_Documents ON Biblio_AuthorsDocuments.IdDocument=Biblio_Documents.IdDocument
		) AS DocInfo ON Biblio_Authors.IdAuthor=DocInfo.IdAuthor
		WHERE %s
		GROUP BY Biblio_Authors.IdAuthor
		ORDER BY %s DESC",
		ifelse(is.null(names.like), "1", sprintf("Name LIKE '%s'", names.like)),
		sqlSwitchOrNULL(orderResultsBy, c("citations", "ndocuments", "name"), c("Citations", "Documents", "Name"))
	);
			
	authors <- dbGetQuery(conn, query);
	n <- nrow(authors);
	
	if (n <= 1) return(NULL);
	
	authors$Group <- NA;
	authors$Name  <- toupper(authors$Name);
	authors$Row   <- 1:n;
	
	
	words <- .lbsFindDuplicateAuthors_split2Words(authors$Name, ignoreWords, minWordLength);
	
	window <- CITAN:::.gtk2.progressBar(0, n, info=sprintf("Generating dependency graph for %g author names... ",n));
	
	rel <- list();
	length(rel) <- n;
	for (i in 1:n)
	{
		rel[[i]] <- i;
		
		words_i <- words[[i]];
		nw <- length(words_i);

		if (nw > 0)
		{
			for (j in 1:nw)
			{
				if (aggressiveness == 0)
				{
					idx <- grep(words_i[j], authors$Name, fixed=TRUE);
				} else
				{
					idx <- agrep(words_i[j], authors$Name);
					if (aggressiveness == 1)
					{
						if (length(idx) > 10)
						{
							idx2 <- grep(words_i[j], authors$Name, fixed=TRUE);
							if (length(idx)/length(idx2)>1.5)
								idx <- idx2;
						}
					}
				}
				
				if (length(idx) > 0)
				{
					rel[[i]] <- c(rel[[i]],idx);
				}
			}
			rel[[i]] <- unique(rel[[i]]);
		}
		
		CITAN:::.gtk2.progressBar(i,n,window=window);
	}

	names(rel) <- authors$IdAuthor;
	return(rel);
}


.lbsFindDuplicateAuthors_getSimClusters <- function(x, y)
{
	xy <- c(x, y);
	nxcapb <- length(x)+length(y)-length(unique(xy));
	
	return(max(nxcapb/length(x),nxcapb/length(y)));
}


.lbsFindDuplicateAuthors_getDupsFromGraph <- function(graph)
{
	dups <- list();
	k <- 1;
	n <- length(graph);
	
	window <- CITAN:::.gtk2.progressBar(0, n, info=sprintf("Trying to group %g author names... ",n));
	selected <- rep(FALSE, n);
	
	for (i in 1:n)
	{
		graph_i <- graph[[i]];
		n_i <- length(graph_i);

		if ((n_i > 1) && (!selected[i]))
		{
			stopifnot(graph_i[1] == i);
			dups[[k]] <- graph_i;
			
			for (j in 2:n_i)
			{
				if (length(unique(c(dups[[k]], graph[[ graph_i[j] ]]))) > 15) next;
				# try to merge the clusters
				if (.lbsFindDuplicateAuthors_getSimClusters(dups[[k]], graph[[ graph_i[j] ]]) > 0.6)
				{
					dups[[k]] <- unique(c(dups[[k]], graph[[ graph_i[j] ]]));
					selected[j] <- TRUE;
				}
			}
			k <- k+1;
		}
		selected[i] <- TRUE;
		

		
		CITAN:::.gtk2.progressBar(i, n, window=window);
	}
	
	x <- sapply(dups, function(x) length(x));
	
	if (k-1 == 0) return(list());
	
	for (i in 1:(k-1))
	{
		dups[[i]] <- as.numeric(names(graph)[dups[[i]]]);
	}
	
	return(dups);
}



#' This function suggests the user some groups of authors that possibly should be merged.
#' It bases on authors' names similarity comparisons.
#'
#' It uses a heuristic **EXPERIMENTAL** algorithm, which behavior
#' is controlled by the \code{aggressiveness} parameter.
#'
#' The search results are presented in a convenient-to-use graphical dialog box.
#' Note that the calculation may take a few minutes!
#' 
#' \code{names.like} is a set of search patterns in an SQL \code{LIKE} format,
#' i.e. an underscore \code{_} matches a single character and a percent sign
#' \code{\%} matches any set of characters. The search is case-insensitive. 
#' 
#' @title Suggest groups of authors to be merged (**EXPERIMENTAL**)
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param names.like a character vector of SQL-LIKE patterns that allow for restricting
#' the search procedure to only given authors' names.
#' @param ignoreWords character vector; words to be ignored.
#' @param minWordLength numeric; minimal word length to be considered.
#' @param orderResultsBy determines results' presentation order; one of
#'    \code{citations}, \code{ndocuments} \code{name}.
#' @param aggressiveness nonnegative integer; controls the search depth.
#' @return
#' List of authors' identifiers to be merged.
#' The first element of each vector is the one marked by the user as \emph{Parent},
#' and the rest are the \emph{Children}.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' listauth <- lbsFindDuplicateAuthors(conn,
#'    ignoreWords=c("van", "von", "der", "no", "author", "name", "available"),
#'    minWordLength=4,
#'    orderResultsBy=c("citations"),
#'    aggressiveness=1);
#' lbsMergeAuthors(conn, listauth);
#' dbCommit(conn);
#' ## ...}
#' @seealso \code{\link{lbsMergeAuthors}}, \code{\link{lbsFindDuplicateTitles}}, \code{\link{lbsGetInfoAuthors}}
#' @export
lbsFindDuplicateAuthors <- function(conn,
	names.like=NULL,
	ignoreWords=c("van", "von", "der", "no", "author", "name", "available"),
	minWordLength=4,
	orderResultsBy=c("citations", "ndocuments", "name"),
	aggressiveness=0
)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection
	
	orderResultsBy <- match.arg(orderResultsBy);
	
	if (!is.numeric(aggressiveness) || length(aggressiveness)!=1 || aggressiveness<0)
		stop("incorrect 'aggressiveness'.");
		
	if (!is.null(ignoreWords) && !is.character(ignoreWords))
		stop("incorrect 'ignoreWords'.");
	ignoreWords <- toupper(ignoreWords);
		
	if (!is.null(names.like) && (!is.character(names.like) || length(names.like)!=1))
		stop("incorrect 'names.like'.");
		


	graph <- .lbsFindDuplicateAuthors_getDupsGraph(conn, names.like, ignoreWords, minWordLength, aggressiveness, orderResultsBy);
	if (is.null(graph)) return(NULL);


	dups <- .lbsFindDuplicateAuthors_getDupsFromGraph(graph);
	

	
	n <- length(dups);
	
	if(n == 0) return(NULL);
	
	merged <- list();
	k <- 0;
	for (i in 1:n)
	{
		ret <- CITAN:::.gtk2.selectAuthors(conn, dups[[i]], sprintf("Select authors to merge (stage %g of %g)", i, n));
		
		if (is.null(ret))
			return(merged);

		if (length(ret) > 0)
		{
			k <- k+1;
			merged[[k]] <- ret;
		}
	}
	
	return(merged);
}




#' Deletes given documents from a Local Bibliometric Storage.
#'
#' For safety reasons, an SQL transaction  opened at the beginning of the
#' removal process is not committed (closed) automatically.
#' You should do it on your own (or rollback it), see Examples below.
#'
#' @title Delete given documents
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param idDocuments a list of numeric vectors or a numeric vector;
#' document identifiers (see \code{IdDocument} in the table \code{Biblio_Documents})
#' to be deleted.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' listdoc <- lbsFindDuplicateTitles(conn,
#'    ignoreTitles.like=c("In this issue\%", "\%Editorial", "\%Introduction",
#'    "\%In this issue", "Letter to \%", "\%Preface"),
#'    aggressiveness=2);
#' lbsDeleteDocuments(conn, listdoc);
#' dbCommit(conn);
#' ## ...}
#' @return
#' \code{TRUE} on success.
#' @seealso \code{\link{lbsGetInfoDocuments}}, \code{\link{lbsFindDuplicateTitles}}
#' @export
lbsDeleteDocuments <- function(conn, idDocuments)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection

	idDocuments <- unlist(idDocuments);
	if (!is.numeric(idDocuments) || length(idDocuments) == 0)
		stop("incorrect 'idDocuments'.");
	
	dbBeginTransaction(conn);
		
	for (i in 1:length(idDocuments))
	{
		dbExecQuery(conn, sprintf("DELETE FROM Biblio_DocumentsSurveys WHERE IdDocument=%g", idDocuments[i]));
		dbExecQuery(conn, sprintf("DELETE FROM Biblio_AuthorsDocuments WHERE IdDocument=%g", idDocuments[i]));
		dbExecQuery(conn, sprintf("DELETE FROM Biblio_Documents WHERE IdDocument=%g", idDocuments[i]));
	}
	
	dbExecQuery(conn,
		"DELETE FROM Biblio_Authors
		WHERE IdAuthor IN (
			SELECT DISTINCT Biblio_Authors.IdAuthor FROM Biblio_Authors
			LEFT JOIN Biblio_AuthorsDocuments ON Biblio_Authors.IdAuthor=Biblio_AuthorsDocuments.IdAuthor
			WHERE Biblio_AuthorsDocuments.IdDocument IS NULL
		)");
		
	warning("Transaction has not been committed yet. Do-it-yourself with dbCommit(...).");
		
	return(TRUE);
}



#' Merges given sets of authors. For each group, the function
#' maps all the related documents to a
#' distinguished \emph{parent} author (the first in a list) and
#' removes the other, unused from then on, records (\emph{children}).
#'
#' This function is useful when one author is represented by many
#' records in a Local Bibliometric Storage (a typical situation in case of
#' data gathered from on-line bibliographic databases),
#' e.g. prof. John Thomas Smith
#' appears as 'Smith J.' and 'Smith J.T.'. Some merge procedures
#' are often absolutely necessary if we would like to assess the
#' impact of authors reliably.
#'
#' Note that you may use \code{\link{lbsFindDuplicateAuthors}}
#' to generate input to this function. It will try to suggest which
#' records should be merged (see Examples below). 
#'
#' For safety reasons, an SQL transaction  opened at the beginning of the
#' removal process is not committed (closed) automatically.
#' You should do it on your own (or rollback it), see Examples below.
#'
#' @title Merge given authors
#' @param conn a connection object as produced by \code{\link{lbsConnect}}.
#' @param idAuthors list of numeric vectors, each consisting of at least 2 authors' identifiers
#' (see \code{IdAuthor} in the table \code{Biblio_Authors});
#' every first element of a vector becomes a \emph{parent} to which other
#' records are merged.
#' @examples
#' \dontrun{
#' conn <- lbsConnect("Bibliometrics.db");
#' ## ...
#' listauth <- lbsFindDuplicateAuthors(conn,
#'    ignoreWords=c("van", "von", "der", "no", "author", "name", "available"),
#'    minWordLength=4,
#'    orderResultsBy=c("citations"),
#'    aggressiveness=1);
#' lbsMergeAuthors(conn, listauth);
#' dbCommit(conn);
#' ## ...}
#' @return
#' \code{TRUE} on success.
#' @seealso \code{\link{lbsFindDuplicateAuthors}}, \code{\link{lbsGetInfoAuthors}}, \code{\link{lbsAssess}}
#' @export
lbsMergeAuthors <- function(conn, idAuthors)
{
	CITAN:::.lbsCheckConnection(conn); # will stop on invalid/dead connection

	if (!is.list(idAuthors) || length(idAuthors) == 0 || !is.numeric(unlist(idAuthors)))
		stop("incorrect 'idAuthors'.");
	
	dbBeginTransaction(conn);
		
	for (i in 1:length(idAuthors))
	{
		stopifnot(length(idAuthors[[i]]) >= 2);
		for (j in 2:length(idAuthors[[i]]))
		{
			dbExecQuery(conn, sprintf("UPDATE Biblio_AuthorsDocuments SET IdAuthor=%g WHERE IdAuthor=%g", idAuthors[[i]][1], idAuthors[[i]][j]));
			dbExecQuery(conn, sprintf("DELETE FROM Biblio_Authors WHERE IdAuthor=%g", idAuthors[[i]][j]));
		}
	}
			
	warning("Transaction has not been committed yet. Do-it-yourself with dbCommit(...).");
		
	return(TRUE);
}
