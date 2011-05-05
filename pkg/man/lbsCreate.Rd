\name{lbsCreate}
\alias{lbsCreate}
\title{Establish a Local Bibliometric Storage}
\usage{lbsCreate(conn, verbose=TRUE)}
\description{Creates an empty Local Bibliometric Storage.}
\details{The function may be executed only if the database does not contain any tables
named \code{Biblio_*} or views named \code{ViewBiblio_*}.

This function creates database tables using the following SQL code.
\preformatted{
CREATE TABLE Biblio_Categories (
-- Source classification codes (e.g. ASJC)
IdCategory        INTEGER PRIMARY KEY ASC,
IdCategoryGroup   INTEGER NOT NULL,
Description       VARCHAR(63) NOT NULL,
FOREIGN KEY(IdCategoryGroup) REFERENCES Biblio_Categories(IdCategory)
);
}

\preformatted{
CREATE TABLE Biblio_Countries (
IdCountry         INTEGER PRIMARY KEY,
Name              VARCHAR(63) NOT NULL UNIQUE
);
}

\preformatted{
CREATE TABLE Biblio_Sources (
IdSource      INTEGER PRIMARY KEY AUTOINCREMENT,
Title         VARCHAR(255) NOT NULL,
ISSN_Print    CHAR(8) UNIQUE CHECK
(length(ISSN_Print)=8 OR length(ISSN_Print) IS NULL),
ISSN_E        CHAR(8) UNIQUE CHECK
(length(ISSN_E)=8 OR length(ISSN_E) IS NULL),
IsActive      BOOLEAN,
IsOpenAccess  BOOLEAN,
Type          CHAR(2) CHECK (Type IN ('bs', 'cp', 'jo')),
-- Book Series / Conference Proceedings / Journal
-- or NULL in all other cases
IdCountry     INTEGER,
Impact        REAL, -- current value of an impact factor
FOREIGN KEY(IdCountry) REFERENCES Biblio_Countries(IdCountry)
);
}

\preformatted{
CREATE TABLE Biblio_SourcesCategories (
-- links Sources and Categories
IdSource         INTEGER NOT NULL,
IdCategory       INTEGER NOT NULL,
PRIMARY KEY(IdSource, IdCategory),
FOREIGN KEY(IdSource)     REFERENCES Biblio_Sources(IdSource),
FOREIGN KEY(IdCategory)   REFERENCES Biblio_Categories(IdCategory)
);
}

\preformatted{
CREATE TABLE Biblio_Surveys (
-- each call to lbsImportDocuments() puts a new record here,
-- they may be grouped using 'Description' into so-called 'Surveys'
IdSurvey       INTEGER PRIMARY KEY AUTOINCREMENT,
Description    VARCHAR(63) NOT NULL,   -- survey group name
FileName       VARCHAR(63),            -- original file name
Timestamp      DATETIME                -- date of file import
);
}

\preformatted{
CREATE TABLE Biblio_Languages (
IdLanguage      INTEGER PRIMARY KEY AUTOINCREMENT,
Name            VARCHAR(63) NOT NULL UNIQUE
);
}

\preformatted{
CREATE TABLE Biblio_Documents (
IdDocument     INTEGER PRIMARY KEY AUTOINCREMENT,
IdSource       INTEGER,
IdLanguage     INTEGER,
UniqueId       VARCHAR(31) UNIQUE NOT NULL,
Title          VARCHAR(255) NOT NULL,
BibEntry       VARCHAR(511) NOT NULL,
-- (Source Title,Year,Volume,Issue,Article Number,PageStart,PageEnd)
Year           INTEGER,
Pages          INTEGER,
Citations      INTEGER NOT NULL,
Type           CHAR(2) CHECK (Type IN ('ar', 'ip', 'bk',
'cp', 'ed', 'er', 'le', 'no', 'rp', 're', 'sh')),
-- Article-ar / Article in Press-ip / Book-bk /
-- Conference Paper-cp / Editorial-ed / Erratum-er /
-- Letter-le/ Note-no / Report-rp / Review-re / Short Survey-sh
-- or NULL in all other cases
FOREIGN KEY(IdSource)   REFERENCES Biblio_Sources(IdSource),
FOREIGN KEY(IdLanguage) REFERENCES Biblio_Languages(IdLanguage)
);
}

\preformatted{
CREATE TABLE Biblio_DocumentsSurveys (
-- note that the one Document may often be found in many Surveys
IdDocument     INTEGER NOT NULL,
IdSurvey       INTEGER NOT NULL,
PRIMARY KEY(IdDocument, IdSurvey),
FOREIGN KEY(IdSurvey)   REFERENCES Biblio_Surveys(IdSurvey),
FOREIGN KEY(IdDocument) REFERENCES Biblio_Documents(IdDocument)
);
}

\preformatted{
CREATE TABLE Biblio_Authors (
IdAuthor        INTEGER PRIMARY KEY AUTOINCREMENT,
Name            VARCHAR(63) NOT NULL UNIQUE
);
}

\preformatted{
CREATE TABLE Biblio_AuthorsDocuments (
-- links Authors and Documents
IdAuthor        INTEGER NOT NULL,
IdDocument      INTEGER NOT NULL,
PRIMARY KEY(IdAuthor, IdDocument),
FOREIGN KEY(IdAuthor)   REFERENCES Biblio_Authors(IdAuthor),
FOREIGN KEY(IdDocument) REFERENCES Biblio_Documents(IdDocument)
);
}

In addition, the following views are created.
\preformatted{
CREATE VIEW ViewBiblio_DocumentsSurveys AS
SELECT
Biblio_DocumentsSurveys.IdDocument AS IdDocument,
Biblio_DocumentsSurveys.IdSurvey AS IdSurvey,
Biblio_Surveys.Description AS Description,
Biblio_Surveys.Filename AS Filename,
Biblio_Surveys.Timestamp AS Timestamp
FROM Biblio_DocumentsSurveys
JOIN Biblio_Surveys
ON Biblio_DocumentsSurveys.IdSurvey=Biblio_Surveys.IdSurvey;
}

\preformatted{
CREATE VIEW ViewBiblio_DocumentsCategories AS
SELECT
IdDocument AS IdDocument,
DocSrcCat.IdCategory AS IdCategory,
DocSrcCat.Description AS Description,
DocSrcCat.IdCategoryGroup AS IdCategoryGroup,
Biblio_Categories.Description AS DescriptionGroup
FROM
(
SELECT
Biblio_Documents.IdDocument AS IdDocument,
Biblio_SourcesCategories.IdCategory AS IdCategory,
Biblio_Categories.Description AS Description,
Biblio_Categories.IdCategoryGroup AS IdCategoryGroup
FROM Biblio_Documents
JOIN Biblio_SourcesCategories
ON Biblio_Documents.IdSource=Biblio_SourcesCategories.IdSource
JOIN Biblio_Categories
ON Biblio_SourcesCategories.IdCategory=Biblio_Categories.IdCategory
) AS DocSrcCat
JOIN Biblio_Categories
ON DocSrcCat.IdCategoryGroup=Biblio_Categories.IdCategory;
}

\if{html}{\out{<p><img src='../doc/CITAN-lbs.png' alt='Tables in a local bibliometric storage and their relations' /></p>}}{}}
\value{\code{TRUE} on success.}
\seealso{\code{\link{lbsConnect}}, \code{\link{lbsClear}}, \code{\link{Scopus_ImportSources}}, \code{\link{lbsTidy}}}
\arguments{\item{conn}{a connection object as produced by \code{\link{lbsConnect}}.}
\item{verbose}{logical; \code{TRUE} to inform about the progress of database contents' creation.}}
\examples{\dontrun{
conn <- lbsConnect("Bibliometrics.db");
## ...
lbsCreate(conn);
Scopus_ImportSources(conn);
## ...
dbDisconnect(conn);}}
