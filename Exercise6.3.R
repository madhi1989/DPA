hmv = read.csv("Home Market value.csv")
hmv$Market.Value = as.numeric(gsub('[$,]','',hmv$Market.Value))
hmv$Square.Feet = as.numeric(gsub('[,]','',hmv$Square.Feet))

summary(hmv)


cor(hmv)

plot(hmv$Square.Feet, hmv$Market.Value)
plot(hmv$Market.Value, hmv$Square.Feet, xlab = 'Market.Value', ylab = 'Square.Feet', main = 'Market.value Vs. square.feet')
plot(hmv$House.Age, hmv$Square.Feet, xlab = 'House.Age', ylab = 'Square.Feet', main = 'House.Age Vs. square.feet')
plot(hmv$House.Age, hmv$Market.Value, xlab = 'House.Age', ylab = 'Market.value', main = 'House.Age Vs. Market.value')
plot(hmv$Square.Feet, hmv$Market.Value, xlab = 'Square.Feet', ylab = 'Market.value', main = 'Square.Feet Vs. Market.value')


plot(hmv$Square.Feet, hmv$House.Age)
plot(hmv$Square.Feet, hmv$Market.Value)
plot(hmv$Market.Value, hmv$Square.Feet)


plot(hmv)


#------linear model------

# lm.hmv = lm(hmv$Market.Value ~ hmv$Square.Feet + hmv$House.Age)
# 
# lm.hmv
# summary(lm.hmv)
newSqure = c(1650,1500,1800,2200,2400)
newAge = c(26,28,29,30,31)
Z =hmv$Market.Value
X= hmv$Square.Feet
Y = hmv$House.Age
lm.hmv = lm(Z~X+Y)
newdata = data.frame(X=newSqure, Y=newAge)
predictedData = predict(lm.hmv,newdata,level = 0.95,interval = "confidence")
predictedData



plot(predictedData)
