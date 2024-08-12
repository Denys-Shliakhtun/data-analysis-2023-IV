USE Games;

CREATE TABLE Export_Table
(
	Game NVARCHAR(200),
	[Platform] NVARCHAR(200),
	Genre NVARCHAR(200),
	Publisher NVARCHAR(200),	
	Metacritic_score INT,
	Metacritic_user_score INT,
	Steam_positive_ratio INT,
	Global_sales FLOAT
);

INSERT INTO Export_Table SELECT 
	Game.Title, 
	Platform_.Platform_, 
	Genre.Genre, 
	Publisher.Publisher, 
	Fact_Game_Score.Metacritic_score, 
	Fact_Game_Score.Metacritic_user_score, 
	Fact_Game_Score.Steam_positive_ratio, 
	Fact_Game_Score.Global_sales
	FROM Fact_Game_Score FULL OUTER JOIN Game_Release ON Fact_Game_Score.ReleaseID = Game_Release.ID
	FULL OUTER JOIN Publisher ON Game_Release.PublisherID = Publisher.ID
	FULL OUTER JOIN Platform_ ON Platform_.ID = Game_Release.PlatformID
	FULL OUTER JOIN Game ON Game.ID = Game_Release.GameID
	FULL OUTER JOIN Genre ON Genre.ID = Game.GenreID

--STAGE Zone

CREATE TABLE Metacritic
(
	Title NVARCHAR(200),
	Platform_ NVARCHAR(50),
	Release_date DATETIME2,
	Meta_score INT,
	User_score REAL
)

INSERT INTO Metacritic SELECT name, platform, release_date, meta_score, TRY_CONVERT(FLOAT, user_review) FROM Metacritic_temp

CREATE TABLE Sales
(
	Title NVARCHAR(200),
	Platform_ NVARCHAR(50),
	Year_of_release INT,
	Genre  NVARCHAR(50),
	Publisher NVARCHAR(50),
	NA_Sales REAL,
	EU_Sales REAL,
	JP_Sales REAL,
	Other_sales REAL,
	Global_sales REAL,
	Developer NVARCHAR(100)
)

INSERT INTO Sales SELECT Name, Platform, TRY_CONVERT(INT, Year_of_release), Genre, Publisher, 
	TRY_CONVERT(REAL, NA_Sales), TRY_CONVERT(REAL, EU_Sales), TRY_CONVERT(REAL, JP_Sales),
	TRY_CONVERT(REAL, Other_sales), TRY_CONVERT(REAL, Global_sales), Developer FROM Sales_temp

CREATE TABLE Steam
(
	ID INT,
	Title NVARCHAR(200),
	Release_date DATETIME2,
	Rating NVARCHAR(50),
	Positive_ratio INT,
	User_reviews INT,
	Price MONEY
)

INSERT INTO Steam SELECT app_id, title, date_release, rating, 
	positive_ratio, user_reviews, TRY_CONVERT(MONEY, price_final) FROM Steam_temp

SELECT * FROM Metacritic
SELECT * FROM Sales
SELECT * FROM Steam

--Database

CREATE TYPE Text_table AS TABLE
(
	Text_ NVARCHAR(200)
)

--Genre
CREATE TABLE Genre
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Genre NVARCHAR(50) NOT NULL UNIQUE
)

GO
CREATE OR ALTER PROCEDURE Add_genres 
	@table Text_table READONLY
AS
BEGIN
	INSERT INTO Genre(Genre) (SELECT DISTINCT Text_ FROM @table WHERE Text_ IS NOT NULL EXCEPT SELECT Genre FROM Genre)
END
GO

--Developer
CREATE TABLE Developer
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Developer NVARCHAR(100) NOT NULL
)

/*USE Games
SELECT DISTINCT TRIM(VALUE) FROM Sales CROSS APPLY STRING_SPLIT(Sales.Developer, ',') WHERE Sales.Developer IS NOT NULL*/

GO
CREATE OR ALTER PROCEDURE Add_developers
	@table Text_table READONLY
AS
BEGIN
	INSERT INTO Developer(Developer) (SELECT DISTINCT TRIM(VALUE) FROM @table CROSS APPLY STRING_SPLIT(Text_, ',') WHERE Text_ IS NOT NULL  EXCEPT SELECT Developer FROM Developer)
END
GO

--Game
CREATE TABLE Game
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	GenreID INT REFERENCES Genre(ID),
	Title NVARCHAR(200) NOT NULL
)

GO
CREATE OR ALTER PROCEDURE Add_games
AS
BEGIN
	INSERT INTO Game(Title, GenreID) ((SELECT DISTINCT Title, Genre.ID FROM Sales JOIN Genre ON Sales.Genre = Genre.Genre WHERE Title IS NOT NULL) EXCEPT (SELECT Title, GenreID FROM Game))
END
GO

--Platform_
CREATE TABLE Platform_
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Platform_ NVARCHAR(50) NOT NULL unique
)

GO
CREATE OR ALTER PROCEDURE Add_platforms
	@table Text_table READONLY
AS
BEGIN
	INSERT INTO Platform_(Platform_) (SELECT DISTINCT Text_ FROM @table WHERE Text_ IS NOT NULL EXCEPT SELECT Platform_ FROM Platform_)
END
GO

DECLARE @table Text_table
INSERT INTO @table SELECT DISTINCT Platform_ FROM Sales
EXEC Add_platforms @table

ALTER TABLE Platform_ ADD Alt_name NVARCHAR(50)
UPDATE Platform_ SET Alt_name = 'PlayStation Vita' WHERE Platform_ = 'PSV'
UPDATE Platform_ SET Alt_name = 'PC' WHERE Platform_ = 'PC'
UPDATE Platform_ SET Alt_name = 'Wii U' WHERE Platform_ = 'WiiU'
UPDATE Platform_ SET Alt_name = 'Nintendo 64' WHERE Platform_ = 'N64'
UPDATE Platform_ SET Alt_name = 'Game Boy Advance' WHERE Platform_ = 'GBA'
UPDATE Platform_ SET Alt_name = 'PlayStation' WHERE Platform_ = 'PS'
UPDATE Platform_ SET Alt_name = 'PlayStation 3' WHERE Platform_ = 'PS3'
UPDATE Platform_ SET Alt_name = 'Wii' WHERE Platform_ = 'Wii'
UPDATE Platform_ SET Alt_name = 'PlayStation 2' WHERE Platform_ = 'PS2'
UPDATE Platform_ SET Alt_name = 'PSP' WHERE Platform_ = 'PSP'
UPDATE Platform_ SET Alt_name = '3DS' WHERE Platform_ = '3DS'
UPDATE Platform_ SET Alt_name = 'Xbox 360' WHERE Platform_ = 'X360'
UPDATE Platform_ SET Alt_name = 'Dreamcast' WHERE Platform_ = 'DC'
UPDATE Platform_ SET Alt_name = 'PlayStation 4' WHERE Platform_ = 'PS4'
UPDATE Platform_ SET Alt_name = 'Xbox' WHERE Platform_ = 'XB'
UPDATE Platform_ SET Alt_name = 'Xbox One' WHERE Platform_ = 'XOne'
UPDATE Platform_ SET Alt_name = 'DS' WHERE Platform_ = 'DS'

--Publisher
CREATE TABLE Publisher
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Publisher NVARCHAR(50) NOT NULL unique
)

GO
CREATE OR ALTER PROCEDURE Add_publishers 
	@table Text_table READONLY
AS
BEGIN
	INSERT INTO Publisher(Publisher) (SELECT DISTINCT Text_ FROM @table WHERE Text_ IS NOT NULL EXCEPT SELECT Publisher FROM Publisher)
END
GO

--Date
CREATE TABLE Date_
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Year_ INT NOT NULL,
	Month_ INT,
	Day_ INT
)

GO
CREATE OR ALTER PROCEDURE Add_dates
AS
BEGIN
	INSERT INTO Date_(Year_) 
		(SELECT DISTINCT Year_of_release FROM Sales 
		WHERE Year_of_release IS NOT NULL
		EXCEPT SELECT Year_ FROM Date_ WHERE Month_ IS NULL AND Day_ IS NULL)
	INSERT INTO Date_(Year_, Month_, Day_) 
		(SELECT DISTINCT DATEPART(YEAR, Release_date), 
		DATEPART(MONTH, Release_date), 
		DATEPART(DAY, Release_date) 
		FROM Metacritic WHERE Release_date IS NOT NULL 
		EXCEPT SELECT Year_, Month_, Day_ FROM Date_)
	INSERT INTO Date_(Year_, Month_, Day_) 
		(SELECT DISTINCT DATEPART(YEAR, Release_date), 
		DATEPART(MONTH, Release_date), 
		DATEPART(DAY, Release_date) 
		FROM Steam WHERE Release_date IS NOT NULL 
		EXCEPT SELECT Year_, Month_, Day_ FROM Date_)
END
GO

--Game_Release
CREATE TABLE Game_Release
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	GameID INT REFERENCES Game(ID) NOT NULL,
	PlatformID INT REFERENCES Platform_(ID),
	Release_date INT REFERENCES Date_(ID),
	PublisherID INT REFERENCES Publisher(ID)
)

GO
CREATE OR ALTER PROCEDURE Add_releases
AS
BEGIN
	INSERT INTO Game_Release(GameID, PlatformID, PublisherID)
		SELECT DISTINCT Game.ID, Platform_.ID, Publisher.ID FROM Game 
		JOIN Sales ON Game.Title = Sales.Title 
		LEFT JOIN Platform_ ON Sales.Platform_ = Platform_.Platform_
		LEFT JOIN Publisher ON Sales.Publisher = Publisher.Publisher
		EXCEPT SELECT GameID, PlatformID, PublisherID FROM Game_Release	
	UPDATE Game_Release SET Release_date = Date_.ID
		FROM Game_Release 
		JOIN Game ON Game_Release.GameID = Game.ID
		JOIN Platform_ ON Game_Release.PlatformID = Platform_.ID
		JOIN Metacritic ON lower(Metacritic.Title) = lower(Game.Title)
		JOIN Date_ ON Date_.Year_ = DATEPART(YEAR, Metacritic.Release_date) 
		AND Date_.MONTH_ = DATEPART(MONTH, Metacritic.Release_date) 
		AND Date_.day_ = DATEPART(DAY, Metacritic.Release_date)
	UPDATE Game_Release SET Release_date = Date_.ID
		FROM Game_Release 
		JOIN Game ON Game_Release.GameID = Game.ID
		JOIN Platform_ ON Game_Release.PlatformID = Platform_.ID
		JOIN Steam ON lower(replace(replace(Steam.Title, '™', ''), '®', '')) = lower(Game.Title)
		JOIN Date_ ON Date_.Year_ = DATEPART(YEAR, Steam.Release_date) 
		AND Date_.MONTH_ = DATEPART(MONTH, Steam.Release_date) 
		AND Date_.day_ = DATEPART(DAY, Steam.Release_date)
		WHERE Game_Release.Release_date IS NULL
	UPDATE Game_Release SET Release_date = Date_.ID
		FROM Game_Release 
		JOIN Game ON Game_Release.GameID = Game.ID
		JOIN Platform_ ON Game_Release.PlatformID = Platform_.ID
		JOIN Sales ON Sales.Title = Game.Title
		JOIN Date_ ON Date_.Year_ = Sales.Year_of_release
		AND Date_.MONTH_ IS NULL
		AND Date_.day_ IS NULL
		WHERE Game_Release.Release_date IS NULL
END

--Game_to_Dev
CREATE TABLE Game_to_Dev
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	ReleaseID INT REFERENCES Game_Release(ID),
	DeveloperID INT REFERENCES Developer(ID)
)

GO
CREATE OR ALTER PROCEDURE Add_Game_to_Dev
AS
BEGIN
	INSERT INTO Game_to_Dev(ReleaseID, DeveloperID) 
		(SELECT DISTINCT Game_Release.ID, Developer.ID FROM Game 
		JOIN Game_Release ON Game.ID = Game_Release.GameID		
		JOIN (SELECT DISTINCT Title, Genre, Platform_, TRIM(VALUE) AS Dev FROM Sales CROSS APPLY STRING_SPLIT(Developer, ','))tabl2
		ON Game.Title = tabl2.Title 
		and tabl2.Genre = (SELECT Genre FROM Genre WHERE ID = Game.GenreID) 
		AND tabl2.Platform_ = (SELECT Platform_ FROM Platform_ WHERE ID = Game_Release.PlatformID)
		JOIN Developer ON Developer.Developer = tabl2.Dev
		) 
		EXCEPT SELECT ReleaseID, DeveloperID FROM Game_to_Dev
END
GO

--Steam_Review
CREATE TABLE Steam_Review
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Steam_Review NVARCHAR(50) NOT NULL unique
)

GO
CREATE OR ALTER PROCEDURE Add_steam_review 
	@table Text_table READONLY
AS
BEGIN
	INSERT INTO Steam_Review(Steam_Review) (SELECT DISTINCT Text_ FROM @table WHERE Text_ IS NOT NULL EXCEPT SELECT Steam_Review FROM Steam_Review)
END
GO

--Region
CREATE TABLE Region
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	Region NVARCHAR(50) NOT NULL unique
)

INSERT INTO Region(Region) VALUES('NA')
INSERT INTO Region(Region) VALUES('EU')
INSERT INTO Region(Region) VALUES('JP')
INSERT INTO Region(Region) VALUES('Other')
INSERT INTO Region(Region) VALUES('Global')

--Fact_Regional_Sales
CREATE TABLE Fact_Regional_Sales
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	ReleaseID INT REFERENCES Game_Release(ID),
	RegionID INT REFERENCES Region(ID),
	Sales REAL
)

GO
CREATE OR ALTER PROCEDURE Add_regional_sales
AS
BEGIN
	INSERT INTO Fact_Regional_Sales(ReleaseID, RegionID, Sales)
		SELECT DISTINCT Game_Release.ID, (SELECT ID FROM Region WHERE Region = 'NA'), Sales.NA_Sales FROM Sales
			JOIN Game_Release ON Sales.Title = (SELECT Game.Title FROM Game WHERE Game_Release.GameID = Game.ID)
			AND Sales.Platform_ = (SELECT Platform_ FROM Platform_ WHERE Game_Release.PlatformID = Platform_.ID)
		UNION SELECT DISTINCT Game_Release.ID, (SELECT ID FROM Region WHERE Region = 'EU'), Sales.EU_Sales FROM Sales
			JOIN Game_Release ON Sales.Title = (SELECT Game.Title FROM Game WHERE Game_Release.GameID = Game.ID)
			AND Sales.Platform_ = (SELECT Platform_ FROM Platform_ WHERE Game_Release.PlatformID = Platform_.ID)
		UNION SELECT DISTINCT Game_Release.ID, (SELECT ID FROM Region WHERE Region = 'JP'), Sales.JP_Sales FROM Sales
			JOIN Game_Release ON Sales.Title = (SELECT Game.Title FROM Game WHERE Game_Release.GameID = Game.ID)
			AND Sales.Platform_ = (SELECT Platform_ FROM Platform_ WHERE Game_Release.PlatformID = Platform_.ID)
		UNION SELECT DISTINCT Game_Release.ID, (SELECT ID FROM Region WHERE Region = 'Other'), Sales.Other_sales FROM Sales
			JOIN Game_Release ON Sales.Title = (SELECT Game.Title FROM Game WHERE Game_Release.GameID = Game.ID)
			AND Sales.Platform_ = (SELECT Platform_ FROM Platform_ WHERE Game_Release.PlatformID = Platform_.ID)
		UNION SELECT DISTINCT Game_Release.ID, (SELECT ID FROM Region WHERE Region = 'Global'), Sales.Global_sales FROM Sales
			JOIN Game_Release ON Sales.Title = (SELECT Game.Title FROM Game WHERE Game_Release.GameID = Game.ID)
			AND Sales.Platform_ = (SELECT Platform_ FROM Platform_ WHERE Game_Release.PlatformID = Platform_.ID)
		EXCEPT SELECT ReleaseID, RegionID, Sales FROM Fact_Regional_Sales
END
GO

--Fact_Game_Score

CREATE TABLE Fact_Game_Score
(
	ID INT IDENTITY(1, 1) PRIMARY KEY,
	ReleaseID INT REFERENCES Game_Release(ID),
	Steam_ReviewID INT REFERENCES Steam_review(ID),
	Global_sales REAL,
	Metacritic_score INT,
	Metacritic_user_score REAL,
	Steam_positive_ratio INT
)

GO
CREATE OR ALTER PROCEDURE Add_game_scores
AS
BEGIN
	INSERT INTO Fact_Game_Score(ReleaseID, Steam_ReviewID, Global_sales, 
	Metacritic_score, Metacritic_user_score, Steam_positive_ratio)
		SELECT Game_Release.ID, Steam_Review.ID, Sales.Global_sales, 
		Metacritic.Meta_score, Metacritic.User_score, Steam.Positive_ratio
		FROM Game_Release 
		JOIN Game ON Game_Release.GameID = Game.ID
		JOIN Platform_ ON Game_Release.PlatformID = Platform_.ID
		JOIN Sales ON Sales.Title = Game.Title
		AND Sales.Platform_ = Platform_.Platform_		
		JOIN Metacritic ON lower(Metacritic.Title) = lower(Game.Title)
		AND Metacritic.Platform_ = Platform_.Alt_name
		LEFT JOIN Steam ON lower(replace(replace(Steam.Title, '™', ''), '®', '')) = lower(Game.Title)
		LEFT JOIN Steam_Review ON Steam.Rating = Steam_Review.Steam_Review
	EXCEPT SELECT ReleaseID, Steam_ReviewID, Global_sales, Metacritic_score, 
		Metacritic_user_score, Steam_positive_ratio FROM Fact_Game_Score		
END
GO

-- QUERIES
--Genre
DECLARE @table Text_table
INSERT INTO @table SELECT DISTINCT Genre FROM Sales
EXEC Add_genres @table

SELECT * FROM Genre
GO

--Developer
DECLARE @table Text_table
INSERT INTO @table SELECT DISTINCT Developer FROM Sales
EXEC Add_developers @table

SELECT * FROM Developer
GO

--Game
EXEC Add_games

SELECT Game.ID, Game.Title, Genre.Genre FROM Game JOIN Genre ON GenreID = Genre.ID

--Platform_
SELECT * FROM Platform_

--Publisher
DECLARE @table Text_table
INSERT INTO @table SELECT DISTINCT Publisher FROM Sales
EXEC Add_publishers @table

SELECT * FROM Publisher

--Date
EXEC Add_dates

SELECT * FROM Date_ order by Year_, Month_, Day_

--Game_Release
EXEC Add_releases

SELECT Game.Title, Platform_.Platform_, Date_.Year_, Date_.Month_, Date_.Day_, Publisher.Publisher 
	FROM Game_Release JOIN Game ON GameID = Game.ID
	LEFT JOIN Platform_ ON Platform_.ID = PlatformID
	LEFT JOIN Publisher ON PublisherID = Publisher.ID	
	LEFT JOIN Date_ ON DATE_.ID = Release_date

--Game_to_Dev
EXEC Add_Game_to_Dev

SELECT Game.Title, Game_Release.ID, Developer, Genre.Genre, Platform_.Platform_ FROM Game
	JOIN Game_Release ON Game.ID = GameID
	JOIN Game_to_Dev ON Game_Release.ID = Game_to_Dev.ReleaseID
	JOIN Developer ON Developer.ID = DeveloperID
	JOIN Genre ON Game.GenreID = Genre.ID
	JOIN Platform_ ON Platform_.ID = PlatformID
	where Game.Title = 'Sword Art Online: Lost Song' OR Game.Title = 'Max Payne'
	order by Game_Release.ID

SELECT * FROM Sales WHERE Title LIKE 'Max Payne' OR Title = 'Sword Art Online: Lost Song'
	
--Steam_Review
GO
DECLARE @table Text_table
INSERT INTO @table SELECT DISTINCT Rating FROM Steam
EXEC Add_steam_review @table

SELECT * FROM Steam_Review
GO

--Region
SELECT * FROM Region

--Fact_Regional_Sales
EXEC Add_regional_sales

SELECT * FROM Sales WHERE Title = 'Mobile Suit Gundam: Crossfire'

SELECT Game.Title, Game.ID, Platform_.Platform_, Publisher.Publisher, Region.Region, Fact_Regional_Sales.Sales 
	FROM Fact_Regional_Sales
	JOIN Game_Release ON Fact_Regional_Sales.ReleaseID = Game_Release.ID
	JOIN Game ON Game_Release.GameID = Game.ID
	JOIN Platform_ ON Game_Release.PlatformID = Platform_.ID
	JOIN Publisher ON Game_Release.PublisherID = Publisher.ID
	JOIN Region ON Region.ID = Fact_Regional_Sales.RegionID
	WHERE Title = 'Mobile Suit Gundam: Crossfire'

--Fact_Game_Score
EXEC Add_game_scores
USE Games
SELECT Game.Title, Genre.Genre, Platform_.Platform_, Publisher.Publisher, 
	Fact_Game_Score.Global_sales, Fact_Game_Score.Metacritic_score, 
	Fact_Game_Score.Metacritic_user_score, 
	Steam_Review.Steam_Review, Fact_Game_Score.Steam_positive_ratio
	FROM Fact_Game_Score LEFT JOIN Game_Release ON ReleaseID = Game_Release.ID
	LEFT JOIN Platform_ ON PlatformID = Platform_.ID
	LEFT JOIN Steam_Review ON Steam_Review.ID = Fact_Game_Score.Steam_ReviewID
	LEFT JOIN Game ON Game.ID = Game_Release.GameID
	LEFT JOIN Genre ON Game.GenreID = Genre.ID
	LEFT JOIN Publisher ON Publisher.ID = PublisherID
	ORDER BY Fact_Game_Score.Steam_positive_ratio DESC
