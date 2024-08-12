USE Games;

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
	