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



#' /internal/
.gtk2.selectDocuments <- function(conn, idDocuments, title="List of documents", remove=FALSE, parent=NULL)
{
	stopifnot(is.numeric(idDocuments))
	n <- length(idDocuments);
	if (remove) stopifnot(n > 1);
	
	if (n>2) {
		window <- CITAN:::.gtk2.progressBar(0, n, info="Preparing data...");
	} else window <- NULL;
	
	info <- list();
	for (i in 1:n)
	{
		info[[i]] <- lbsGetInfoDocuments(conn, idDocuments[i])[[1]];
		if (!is.null(window)) CITAN:::.gtk2.progressBar(i, n, window=window);
	}


	if (remove)
	{
		dialog <- gtkDialogNewWithButtons(NULL, parent, 0,
			"Remove selected", 1,
			"Do nothing", 0,
			"gtk-cancel", GtkResponseType["reject"], show=FALSE);
	} else
	{
			dialog <- gtkDialogNewWithButtons(NULL, parent, 0,
			"gtk-ok", GtkResponseType["ok"], show=FALSE);
	}
	dialog$setDefaultSize(900, 400);
	dialog$setTitle(title);
	
	data <- data.frame(
		"Del"       =FALSE,
		"Id"        =sapply(info, function(x) x$IdDocument),
		"Title"     =sapply(info, function(x) x$Title),
		"BibEntry"  =sapply(info, function(x) x$BibEntry),
		"Year"      =sapply(info, function(x) x$Year),
		"Type"      =sapply(info, function(x) x$Type),
		"Cit."      =sapply(info, function(x) x$Citations),
		"Authors"   =sapply(info, function(x) {
			paste(sapply(x$Authors,
				function(y) paste(y$Name, y$IdAuthor, sep="/")
			),
			collapse=", ")
		}),
		"UniqueId"  =sapply(info, function(x) x$UniqueId)
	);
	
	if (!remove) data <- data[,-1];

	model <- rGtkDataFrame(data);
	tree_view <- gtkTreeView(model);
	
	sapply(1:ncol(model), function(j) {
		if (j == 1 && remove)
		{
			renderer <- gtkCellRendererToggle();
			renderer[["radio"]] <- FALSE;
			column <- gtkTreeViewColumn(colnames(model)[j], renderer, active = j-1)
			tree_view$appendColumn(column)
			gSignalConnect(renderer, "toggled", function(widget, path)
			{
				model[as.integer(path)+1][1]$Del <<- !model[as.integer(path)+1][1]$Del;
			})
		} else {
			renderer <- gtkCellRendererText();
			renderer[["wrap-width"]] <- 250;
			renderer[["wrap-mode"]] <- GtkWrapMode["word"];
			column <- gtkTreeViewColumn(colnames(model)[j], renderer, text = j-1)
			tree_view$appendColumn(column)
		}
	})
	if (is.null(gtkCheckVersion(2, 10, 0))) tree_view$setGridLines("both");



	swin <- gtkScrolledWindow()
	swin$add(tree_view);
	dialog[["vbox"]]$add(swin);
	
	doneVal <- dialog$run();
	
	dialog$destroy();
	
	data <- as.data.frame(model);


	if (doneVal==1) {
		return(data$Id[data$Del]);
	} else if (doneVal<0) {
		return(NULL);
	} else return(integer(0));
}




#' /internal/
.gtk2.selectAuthors <- function(conn, idAuthors, title="Select authors to merge")
{
	stopifnot(length(idAuthors) > 1 && is.numeric(idAuthors));
	n <- length(idAuthors);
	
	if (n>2) {
		window <- CITAN:::.gtk2.progressBar(0, n, info="Preparing data...");
	} else window <- NULL;
	
	info <- list();
	length(info) <- n;
	for (i in 1:n)
	{
		info[[i]] <- as.list(dbGetQuery(conn, sprintf(
			"SELECT Biblio_Authors.IdAuthor AS IdAuthor, Name,
				COUNT(IdDocument) As Documents,
				SUM(Citations) As Citations
			FROM Biblio_Authors
			JOIN (
				SELECT Biblio_AuthorsDocuments.IdAuthor, Biblio_AuthorsDocuments.IdDocument, Biblio_Documents.Citations
				FROM Biblio_AuthorsDocuments
				JOIN Biblio_Documents ON Biblio_AuthorsDocuments.IdDocument=Biblio_Documents.IdDocument
			) AS DocInfo ON Biblio_Authors.IdAuthor=DocInfo.IdAuthor
			WHERE Biblio_Authors.IdAuthor=%g
			GROUP BY Biblio_Authors.IdAuthor",
		idAuthors[i])));

		if (!is.null(window)) CITAN:::.gtk2.progressBar(i, n, window=window);
	}
	
	info <- info[order(sapply(info, function(x) x$Name))];


	dialog <- gtkDialogNewWithButtons(NULL, NULL, 0,
		"Merge selected", 1,
		"Do nothing", 0,
		"gtk-cancel", GtkResponseType["reject"], show=FALSE);
	dialog$setDefaultSize(700, 400);
	dialog$setTitle(title);
	
	
	data <- data.frame(
		"Parent"     =FALSE,
		"Child"      =FALSE,
		"Id"         =sapply(info, function(x) x$IdAuthor),
		"Name"       =sapply(info, function(x) x$Name),
		"Documents"  =sapply(info, function(x) x$Documents),
		"Citations"  =sapply(info, function(x) x$Citations)
	);

	data$Id <- as.numeric(data$Id);
	model <- rGtkDataFrame(data);
	tree_view <- gtkTreeView(model);
	
	sapply(1:ncol(model), function(j) {
		if (j == 1)
		{
			renderer <- gtkCellRendererToggle();
			renderer[["radio"]] <- TRUE;
			column <- gtkTreeViewColumn(colnames(model)[j], renderer, active = j-1)
			tree_view$appendColumn(column)
			gSignalConnect(renderer, "toggled", function(widget, path)
			{
				if (model[as.integer(path)+1][1]$Parent == FALSE)
				{
					for (o in 1:n)
						model[o][1]$Parent <<- FALSE;
				}
				model[as.integer(path)+1][1]$Parent <<- TRUE;
				model[as.integer(path)+1][2]$Child <<- FALSE;
			})
		} else if (j == 2)
		{
			renderer <- gtkCellRendererToggle();
			renderer[["radio"]] <- FALSE;
			column <- gtkTreeViewColumn(colnames(model)[j], renderer, active = j-1)
			tree_view$appendColumn(column)
			gSignalConnect(renderer, "toggled", function(widget, path)
			{
				if (!model[as.integer(path)+1][1]$Parent)
					model[as.integer(path)+1][2]$Child <<- !model[as.integer(path)+1][2]$Child;
			})
		} else {
			renderer <- gtkCellRendererText();
			renderer[["wrap-width"]] <- 350;
			renderer[["wrap-mode"]] <- GtkWrapMode["word"];
			column <- gtkTreeViewColumn(colnames(model)[j], renderer, text = j-1)
			tree_view$appendColumn(column)
		}
	})
	
	renderer <- gtkCellRendererToggle();
	renderer[["radio"]] <- TRUE;
	column <- gtkTreeViewColumn("List doc.", renderer)
	tree_view$insertColumn(column,4)
	gSignalConnect(renderer, "toggled", function(widget, path)
	{
		.gtk2.selectDocuments(conn,
			lbsSearchDocuments(conn, idAuthors=model[as.integer(path)+1][3]$Id),
			parent=dialog, remove=FALSE,
			title=sprintf("Documents by %s (IdAuthor=%g)", model[as.integer(path)+1][4]$Name, model[as.integer(path)+1][3]$Id));
	})
	
	if (is.null(gtkCheckVersion(2, 10, 0))) tree_view$setGridLines("both");



	swin <- gtkScrolledWindow()
	swin$add(tree_view);
	dialog[["vbox"]]$add(swin);
	
	doneVal <- dialog$run();
	
	dialog$destroy();
	
	data <- as.data.frame(model);


	if (doneVal==1) {
		x <- c(data$Id[data$Parent], data$Id[data$Child]);
		if (length(x) < 2)
		{
			warning("you should select one parent and at least one child.");
			return(integer(0));
		}
		return(x);
	} else if (doneVal<0) {
		return(NULL);
	} else return(integer(0));
}





#' /internal/
.gtk2.progressBar <- function(i, M, each=max(1,floor(M/100)), info=NULL, window=NULL)
{
	strTimeLeft <- "Estimated time left: %g secs.";

	if (!is.null(info))
	{
		stopifnot(is.null(window));
		
		window <- gtkWindowNew("toplevel", FALSE);
		window$setDefaultSize(250, 60);
		window$setTitle("Operation progress");
		
		box <- gtkVBox(FALSE);
		
		lblTitle <- gtkLabelNew(info);
		barProgress <- gtkProgressBarNew();
		gtkProgressBarSetText(barProgress, "0%");
		gtkProgressBarSetFraction(barProgress, 0.0);
		lblTimeLeft <- gtkLabelNew(sprintf(strTimeLeft, NA));
		
		box$add(lblTitle);
		box$add(barProgress);
		box$add(lblTimeLeft);
		
		gObjectSetData(window, "TimeStart", Sys.time());
		gObjectSetData(window, "lblTitle", lblTitle);
		gObjectSetData(window, "barProgress", barProgress);
		gObjectSetData(window, "lblTimeLeft", lblTimeLeft);
		
		window$add(box);
		window$setResizable(FALSE);
		window$show();

		return(window);
	} else 
	{
		stopifnot(!is.null(window));
	}
	
	if (i==M)
	{
		window$destroy();
		return(NULL);
	} else if (i %% each == 0L)
	{
		TimeStart   <- gObjectGetData(window, "TimeStart");
		lblTitle    <- gObjectGetData(window, "lblTitle");
		barProgress <- gObjectGetData(window, "barProgress");
		lblTimeLeft <- gObjectGetData(window, "lblTimeLeft");
		
		gtkWidgetQueueDraw(lblTitle);
		gtkProgressBarSetFraction(barProgress, i/M);
		gtkProgressBarSetText(barProgress, sprintf("%g%%", floor(i/M*100)));
		gtkLabelSetText(lblTimeLeft, sprintf(strTimeLeft,
			round(as.numeric(Sys.time()-TimeStart, units="secs")*(M-i)/i, 0)
		));
		
		warn <- getOption("warn");
		options("warn"=-1);
		gtkWidgetDraw(window, c(0,0,0,0)); # must update!
		options("warn"=warn);
	}
	
	return(window);
}

