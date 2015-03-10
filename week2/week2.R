## Quick questions

## 1
df = read.csv("wine.csv")
fit = lm(Price ~ HarvestRain + WinterRain, df)
summary(fit)

cor(df$HarvestRain, df$WinterRain)

## 2
bb = read.csv("baseball.csv")
moneyball = subset(bb, Year < 2002)
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)

predict(WinsReg, data.frame(RD=713-614))

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)
predict(RunsReg, data.frame(OBP=0.311,SLG=0.405))

AllowReg = lm(RA ~ OOBP + OSLG, data=moneyball)
predict(AllowReg, data.frame(OOBP=0.297, OSLG=0.370))

players = data.frame(
  Name = c("Eric", "Jeremy", "Frank", "Greg", "Carlos"),
  OBP  = c(0.338, 0.391, 0.369, 0.313, 0.361),
  SLG  = c(0.540, 0.450, 0.374, 0.447, 0.500),
  Salary = c(1.4, 1.065, 0.295, 0.8, 0.3))
players = cbind(players, p = predict(RunsReg, players))
players = merge(players, players, by = NULL)
players$total = players$p.x + players$p.y
players = players[(players$Name.x!=players$Name.y) & (players$Salary.x + players$Salary.y)<=1.5,]
players[order(-players$total),]
