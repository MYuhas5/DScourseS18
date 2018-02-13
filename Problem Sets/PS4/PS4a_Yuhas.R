library("jsonlite")

system('wget -O nfl.json "http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json"')
system('cat nfl.json')

#create data set from json
nfldf <- fromJSON("nfl.json")

#Check data types
class(nfldf)
class(nfldf$players)
#List first n
head(nfldf$players)
