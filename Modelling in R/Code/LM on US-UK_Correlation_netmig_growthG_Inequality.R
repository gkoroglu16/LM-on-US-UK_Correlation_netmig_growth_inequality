setwd('~/Documents/Projects🎓🌁/INDEP_Study_Carlson/Thesis-Indep/Modelling/')
library('readxl')
data = read_excel('DataWDI2.xlsx')
data = data[(1:10), ] #Maybe keep it to only USA and UK for now (2 countries) - since need to do interpolating

"#You should really put interpolation into a generalized function, and impute for all countries. 
#Alternatively, you could keep all countries and not impute since you will have more data anyways. That way you do not have to defend as many assumptions. 
#Regardless, if you stick to just the US and UK, you would need to defend why these countries are important or relevant to your question. 
#Don't worry about getting this done in a timely manner, but as you work towards your goal, 
#you should really be including all relevant data or justifying why you are not."

head(data)
summary(data)

# Check the relationship between these vars:
# 1. GDP per capita growth (annual %) – Net Migration 
# 2. Exports of goods and services (annual % growth) – Net Migration 
# 3. Income share held by lowest 20% - Net Migration 
# 4. Households and NPISHs Final consumption expenditure per capita growth (annual %) – Net Migration

colnames(data)  #identify the colnames, so we can work on them
data = data[, -2] #dont need the first two 2 cols


data = as.data.frame(data)
hold = matrix(ncol = 6) #create 9 cols matrix
for(coun in as.character(unique(data[, 'Country Name']))[1:2]){
    hold1 = data[as.character(data[,'Country Name']) == coun, ]
    hold1 = hold1[,-(1:3)]
    hold1 = t(hold1)
    hold1 = cbind(coun, hold1)
    hold = rbind(hold, hold1)
    }

colnames(hold)
holdcol = unique(data[,'Series Name'])  #|define 'holdcol' to keep 'Series Name'
colnames(hold)[2:6] = holdcol #Put the defined 'Series Name's in 'holdcol' to hold colnames
hold = hold[-1, ]
hold = hold[, 1:6]  #drop the 3 'na' cols at the end


#GDP per capita growth (annual %) – Net Migration  - lmer on data1
data1 = na.omit(hold[, 1:6])


#interpolate the net migration and GDP per capita growth - US (1)
#internet - US
data2 = data1[(1:61),(2:6)]
holdRaws = rownames(data2)
data2 = apply(data2, 2, function(x){as.numeric(x)
                })
row.names(data2) = as.numeric(as.factor(holdRaws)) + 1959 #rename the rawname to years (simple)

dataUS = na.omit(data2[(1:61),1])
par(mfrow = c(1,1))
plot(dataUS ~ as.numeric(names(dataUS)))
data2 = cbind(data2[(1:61),],
              spline(as.numeric(names(dataUS)), dataUS, n = nrow(data2[(1:61),]), xmin = as.numeric(rownames(data2[(1:61),])[1]), xmax = as.numeric(rownames(data2[(1:61),])[nrow(data2[(1:61),])]))$y)
colnames(data2)[6] = 'interNet'
#interGDPGrowth - US
dataUS = na.omit(data2[,2])
par(mfrow = c(1,1))
plot(dataUS ~ as.numeric(names(dataUS)))
data2 = cbind(data2[(1:61),],
              spline(as.numeric(names(dataUS)), dataUS, n = nrow(data2[(1:61),]), xmin = as.numeric(rownames(data2[(1:61),])[1]), xmax = as.numeric(rownames(data2[(1:61),])[nrow(data2[(1:61),])]))$y)
colnames(data2)[7] = 'interGDPGrowth'
#interNetExportsG - US
dataUS = na.omit(data2[,3])
par(mfrow = c(1,1))
plot(dataUS ~ as.numeric(names(dataUS)))
data2 = cbind(data2[(1:61),],
              spline(as.numeric(names(dataUS)), dataUS, n = nrow(data2[(1:61),]), xmin = as.numeric(rownames(data2[(1:61),])[1]), xmax = as.numeric(rownames(data2[(1:61),])[nrow(data2[(1:61),])]))$y)
colnames(data2)[8] = 'interNetExportsG'
#interIncome20 - US
dataUS = na.omit(data2[,4])
par(mfrow = c(1,1))
plot(dataUS ~ as.numeric(names(dataUS)))
data2 = cbind(data2[(1:61),],
              spline(as.numeric(names(dataUS)), dataUS, n = nrow(data2[(1:61),]), xmin = as.numeric(rownames(data2[(1:61),])[1]), xmax = as.numeric(rownames(data2[(1:61),])[nrow(data2[(1:61),])]))$y)
colnames(data2)[9] = 'interIncome20'
#interHousehold Final - US
dataUS = na.omit(data2[,5])
par(mfrow = c(1,1))
plot(dataUS ~ as.numeric(names(dataUS)))
data2 = cbind(data2[(1:61),],
              spline(as.numeric(names(dataUS)), dataUS, n = nrow(data2[(1:61),]), xmin = as.numeric(rownames(data2[(1:61),])[1]), xmax = as.numeric(rownames(data2[(1:61),])[nrow(data2[(1:61),])]))$y)
colnames(data2)[10] = 'interHousehold Final'

data2 = data2[, -(1:5)] #to put interpolated US vars into last shape


#interpolate the net migration and GDP per capita growth - UK (2)
#internet - UK
datab2 = data1[(62:122),(2:6)]
holdRaws = rownames(datab2)
datab2 = apply(datab2, 2, function(x){as.numeric(x)
                    })
row.names(datab2) = as.numeric(as.factor(holdRaws)) + 1959 #rename the rawname to years (simple)

dataUK = na.omit(datab2[, 1])
par(mfrow = c(1,1))
plot(dataUK ~ as.numeric(names(dataUK)))
datab2 = cbind(datab2[(1:61),],
              spline(as.numeric(names(dataUK)), dataUK, n = nrow(datab2[(1:61),]), xmin = as.numeric(rownames(datab2[(1:61),])[1]), xmax = as.numeric(rownames(datab2[(1:61),])[nrow(datab2[(1:61),])]))$y)
colnames(datab2)[6] = 'interNet'
#interGDPGrowth - UK
dataUK = na.omit(datab2[, 2])
par(mfrow = c(1,1))
plot(dataUK ~ as.numeric(names(dataUK)))
datab2 = cbind(datab2[(1:61),],
               spline(as.numeric(names(dataUK)), dataUK, n = nrow(datab2[(1:61),]), xmin = as.numeric(rownames(datab2[(1:61),])[1]), xmax = as.numeric(rownames(datab2[(1:61),])[nrow(datab2[(1:61),])]))$y)
colnames(datab2)[7] = 'interGDPGrowth'
#interNetExportsG - UK
dataUK = na.omit(datab2[, 3])
par(mfrow = c(1,1))
plot(dataUK ~ as.numeric(names(dataUK)))
datab2 = cbind(datab2[(1:61),],
               spline(as.numeric(names(dataUK)), dataUK, n = nrow(datab2[(1:61),]), xmin = as.numeric(rownames(datab2[(1:61),])[1]), xmax = as.numeric(rownames(datab2[(1:61),])[nrow(datab2[(1:61),])]))$y)
colnames(datab2)[8] = 'interGDPGrowth'
#interIncome20 - UK
dataUK = na.omit(datab2[, 4])
par(mfrow = c(1,1))
plot(dataUK ~ as.numeric(names(dataUK)))
datab2 = cbind(datab2[(1:61),],
               spline(as.numeric(names(dataUK)), dataUK, n = nrow(datab2[(1:61),]), xmin = as.numeric(rownames(datab2[(1:61),])[1]), xmax = as.numeric(rownames(datab2[(1:61),])[nrow(datab2[(1:61),])]))$y)
colnames(datab2)[9] = 'interIncome20'
#interHousehold Final - UK
dataUK = na.omit(datab2[, 5])
par(mfrow = c(1,1))
plot(dataUK ~ as.numeric(names(dataUK)))
datab2 = cbind(datab2[(1:61),],
               spline(as.numeric(names(dataUK)), dataUK, n = nrow(datab2[(1:61),]), xmin = as.numeric(rownames(datab2[(1:61),])[1]), xmax = as.numeric(rownames(datab2[(1:61),])[nrow(datab2[(1:61),])]))$y)
colnames(datab2)[10] = 'interHousehold Final'

datab2 = datab2[, -(1:5)]  #to put interpolated UK vars into last shape

#combine all interpolated data2 (for US) and datab2 (for UK) - respectively
data2 = rbind(data2, datab2) # all US and UK interpolated net migration and GDP per capita growth
data1 = cbind(data1, data2) #Transform data2 to the end of data1
data1 = data1[, -(2:6)]


data1 = data.frame(coun = as.factor(data1[,1]),
                   netmig = as.numeric(data1[,2]),
                   GDPGrowthper = as.numeric(data1[,3]),
                   NetExportG = as.numeric(data1[,4]),
                   Income20 = as.numeric(data1[,5]),
                   HouseholdF = as.numeric(data1[,6])
                   )
data1[,(2:6)] = apply(data1[, (2:6)], 2, scale)

#Since out of 13 countries, I dropped it to 2 countries, I ran lm rather than lmer
mod1 = lm(GDPGrowthper  ~ netmig + coun, data = data1)    #???here I could not apply country as location tho???
summary(mod1)
#plot(mod1)
#library(stargazer)
#library(jtools)
#stargazer(mod1, caption ="Regression Results")
#export_summs(mod1, to.file = 'html', caption ="Regression Results")

mod2 = lm(NetExportG  ~ netmig + coun, data = data1) 
summary(mod2)
#stargazer(mod2)
#xport_summs(mod2, to.file = 'html')

mod3 = lm(Income20 ~ netmig + coun, data = data1)
summary(mod3)
#stargazer(mod3)
#export_summs(mod3, to.file = 'html')

mod4 = lm(HouseholdF ~ netmig + coun, data = data1) 
summary(mod4)
#stargazer(mod4)
#export_summs(mod4, to.file = 'html')


#Make a table of the results
library(sjPlot)
library(sjmisc)
library(sjlabelled)
tab_model(
    mod1, mod2, mod3, mod4,
    auto.label = FALSE,
    transform = NULL,
    pred.labels = c("Intercept", "Net Migration", "coun"),
    dv.labels = c("GDP per capita growth (annual %)", "Exports of goods and services (annual % growth)",
                  "Income share held by lowest 20%", "Households and NPISHs Final consumption expenditure per capita growth (annual %)"),
    string.pred = "Coeffcient",
    string.ci = "Conf. Int (95%)",
    string.p = "P-Value",
    p.style = "stars"
    )

---
    
#NOT WORKING SCALING HERE --> (this part for lmer scaling, not lm scaling!)
#data1[, 2:3] = apply(as.numeric(data1[,2:3]), 2, scale)  #2nd try of scaling
#???scale for a better coef - NOT WORKING??? --> BUT in data1, I have no NAs (so I don't need this do I?)
#stdize = function(x, na.rm = T) {(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))}
#x = data1 
#x[data1(1:780, size = 10)] <- NA 
#x = data.frame(x) 
#data1 = apply(data1, MARGIN = 2, FUN = stdize) 
     
#Other lmers (if needed)
library(lme4)
mod1 <- lmer(GDPGrowthper ~ netmig + (1 | coun), data = data1)
summary(mod1)
    
#Exports of goods and services (annual % growth) – Net Migration - lmer- data2
data2 = na.omit(hold[, 1:4])
data2 = data2[,-3]
mod2 = lmer(as.numeric(data2[,3]) ~ as.numeric(data2[,2]) + (1 | data2[,1]))
summary(mod2)

#Income share held by lowest 20% - Net Migration - lmer- data3
data3 = na.omit(hold[, 1:5])
data3 = data3[,-(3:4)]
mod3 = lmer(as.numeric(data3[,3]) ~ as.numeric(data3[,2]) + (1 | data3[,1]))
summary(mod3)

#Households and NPISHs Final consumption expenditure per capita growth (annual %) – Net Migration - lmer - data4
data4 = na.omit(hold[, 1:6])
data4 = data4[,-(3:5)]
mod4 = lmer(as.numeric(data4[,3]) ~ as.numeric(data4[,2]) + (1 | data4[,1]))
summary(mod4)









