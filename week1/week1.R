who = read.csv("WHO.csv")
mean(who$Over60)

head(who[order(who$Over60),])

head(who[order(-who$LiteracyRate),])

data.frame(with(who, tapply(ChildMortality, Region, mean)))


#################
## HW1

mvt = read.csv("mvtWeek1.csv")


data.frame(prop.table(table(mvt$Arrest[mvt$Year==2001])))
data.frame(prop.table(table(mvt$Arrest[mvt$Year==2007])))
data.frame(prop.table(table(mvt$Arrest[mvt$Year==2012])))

tail(data.frame(sort(table(mvt$LocationDescription))))

Top5 = mvt[mvt$LocationDescription %in% 
             c("DRIVEWAY - RESIDENTIAL",
               "GAS STATION",
               "ALLEY",
               "PARKING LOT/GARAGE(NON.RESID.)",
               "STREET")
             ,]
Top5$LocationDescription = factor(Top5$LocationDescription)

arrestrate= as.data.frame.matrix(table(Top5$LocationDescription, Top5$Arrest))
names(arrestrate) = c("no", "yes")
arrestrate$rate = with(arrestrate, yes / (yes + no))


##2

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

dim(IBM)
min(GE$Date)
max(Boeing$Date)

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

plot(StockPrice ~ Date, data=CocaCola, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="black")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="green")
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="blue")
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="Orange")

data.frame(delta=sort(tapply(IBM$StockPrice, months(IBM$Date), mean) - mean(IBM$StockPrice)))
data.frame(delta=sort(tapply(GE$StockPrice, months(GE$Date), mean) - mean(GE$StockPrice)))

##3

CPS = read.csv("CPSData.csv")
summary(CPS)
str(CPS)

data.frame(sort(table(CPS$Industry)))
data.frame(sort(table(CPS$State)))

data.frame(prop.table(table(CPS$Citizenship)))

with(CPS, table(Race, Hispanic))

data.frame(x=sapply(CPS, function(x) sum(is.na(x))))

table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

metro = as.data.frame.matrix(table(CPS$Region, is.na(CPS$MetroAreaCode)))
names(metro) = c("metro", "non.metro")
metro$prop = with(metro, non.metro / (metro + non.metro))
metro[which.max(metro$prop),]

data.frame(x=sort(with(CPS, tapply(is.na(MetroAreaCode), State, mean))))

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

