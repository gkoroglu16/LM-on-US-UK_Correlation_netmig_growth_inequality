setwd('~/Documents/GitHub/QPMRFall2021/inclass/day11application/')
library('readxl')
data = read_excel('DataWDI.xlsx')
head(data)
summary(data)

# Now check the relationship between the vars:
# 1. GDP per capita growth (annual %) – Net Migration 
# 2. Exports of goods and services (annual % growth) – Net Migration 
# 3. Income share held by lowest 20% - Net Migration 
# 4. Households and NPISHs Final consumption expenditure per capita growth (annual %) – Net Migration

colnames(data)  #identify the colnames, so we can work on them
# we dont need the first two 2 col
data = data[, -(1:2)]
data = t(data)  #transpose the data
head(data)  #optional
colnames(data) = data[2, ] #name cols with their series name in the excel
data = data[, !is.na(colnames(data))] #drop 'na's and make your data cleaner 

#Define the vars-interested bf digging into the data
#Net migration
vars = colnames(data)[data['Series Name', ] == 'Net migration']
# GDP per capita growth (annual %)
vars = c(vars, colnames(data)[data['Series Name', ] == 'GDP per capita growth (annual %)'])
# Exports of goods and services (annual % growth)
vars = c(vars, colnames(data)[data['Series Name', ] == 'Exports of goods and services (annual % growth)'])
# Income share held by lowest 20%
vars = c(vars, colnames(data)[data['Series Name', ] == 'Income share held by lowest 20%'])
#Households and NPISHs Final consumption expenditure per capita growth (annual %)
vars = c(vars, colnames(data)[data['Series Name', ] == 'Households and NPISHs Final consumption expenditure per capita growth (annual %)'])

# We dont need first two row
data2 = data[(-(1:2)), vars]
holdRaws = rownames(data2)  #defined 'holdRaws' to name the rows later in Line
#convert every chrac to numeric
data2 = apply(data2, 2, function(x){as.numeric(x)
        })
row.names(data2) = as.numeric(as.factor(holdRaws)) + 1959 #rename the rawname to years (simple)

#plot densities
par(mfrow = c(2,3)) #making the data 2-3 matrix
for(var in vars) plot(density(data2[, var], na.rm = T), main = data['Series Name', var])

#rescale - densities not set correct
holdRaws = rownames(data2)
data2 = apply(data2, 2, scale)
rownames(data2) = holdRaws

# now engage in bivariate relations - gives probabilities for simultaneous outcomes of the two random variable

#run the linear regression w 'LM'
#Linear regression - net mig as explanatory var, other vars as outcome vars
for (var in vars) assign(paste0('mod', var), lm(data2[, 1] ~ data2[, var] - 1))

#store it all in one table and call it 'biLMtab' - 'stargazer' for paper presentation - check DAY6 for tables
biLMtab = matrix(NA, nrow = length(vars) - 1, ncol = 4)
for(i in 1:length(vars[-1])) biLMtab[i, ] = summary(get(paste0('mod', vars[i + 1])))$coef
rownames(biLMtab) = data['Series Name', vars[-1]] #drops the first col in data2
colnames(biLMtab) = colnames(summary(get(paste0('mod', vars[i + 1])))$coef)

#"library(jtools)
#export_summs(pop_logit, to.file = 'docx')"

summary(data2[, 1])
sum(!is.na(data2[, 1])) #problem here -- 12 not-NAs in col1 var in data2

#so lets work on it
data3 = na.omit(data2[, 1])
par(mfrow = c(1,1)) #reset the vars 1-1 matrix
plot(data3 ~ as.numeric(names(data3)))
#interpolate the data (net mig) - interpolating splines
data2 = cbind(data2,
              spline(as.numeric(names(data3)), data3, n = nrow(data2), xmin = as.numeric(rownames(data2)[1]), xmax = as.numeric(rownames(data2)[nrow(data2)]))$y)
colnames(data2)[6] = 'interNet' #create a 6th col named 'internet' (our net mig data in data3 and interpolated with data 2)
plot(data2[, 'interNet'] ~ as.numeric(rownames(data2)))

#interpolate col2 in data2 (gdp growth data)
data3 = na.omit(data2[, 2])
par(mfrow = c(1,1)) #reset the vars 1-1 matrix
plot(data3 ~ as.numeric(names(data3)))
data2 = cbind(data2,
              spline(as.numeric(names(data3)), data3, n = nrow(data2), xmin = as.numeric(rownames(data2)[1]), xmax = as.numeric(rownames(data2)[nrow(data2)]))$y)
colnames(data2)[7] = 'interGDPGrowth' #create a 6th col named 'internet' (our net mig data in data3 and interpolated with data 2)
plot(data2[, 'interGDPGrowth'] ~ as.numeric(rownames(data2)))

#interpolate col3 in data2 (net exports data)
data3 = na.omit(data2[, 3])
par(mfrow = c(1,1)) #reset the vars 1-1 matrix
plot(data3 ~ as.numeric(names(data3)))
data2 = cbind(data2,
              spline(as.numeric(names(data3)), data3, n = nrow(data2), xmin = as.numeric(rownames(data2)[1]), xmax = as.numeric(rownames(data2)[nrow(data2)]))$y)
colnames(data2)[8] = 'interNetExportsG' #create a 6th col named 'internet' (our net mig data in data3 and interpolated with data 2)
plot(data2[, 'interNetExportsG'] ~ as.numeric(rownames(data2)))

#interpolate col4 in data2 (income 20 data)
data3 = na.omit(data2[, 4])
par(mfrow = c(1,1)) #reset the vars 1-1 matrix
plot(data3 ~ as.numeric(names(data3)))
data2 = cbind(data2,
              spline(as.numeric(names(data3)), data3, n = nrow(data2), xmin = as.numeric(rownames(data2)[1]), xmax = as.numeric(rownames(data2)[nrow(data2)]))$y)
colnames(data2)[9] = 'interIncome20' #create a 6th col named 'internet' (our net mig data in data3 and interpolated with data 2)
plot(data2[, 'interIncome20'] ~ as.numeric(rownames(data2)))

#interpolate col5 in data2 (household final data)
data3 = na.omit(data2[, 5])
par(mfrow = c(1,1)) #reset the vars 1-1 matrix
plot(data3 ~ as.numeric(names(data3)))
data2 = cbind(data2,
              spline(as.numeric(names(data3)), data3, n = nrow(data2), xmin = as.numeric(rownames(data2)[1]), xmax = as.numeric(rownames(data2)[nrow(data2)]))$y)
colnames(data2)[10] = 'interHousehold Final' #create a 6th col named 'internet' (our net mig data in data3 and interpolated with data 2)
plot(data2[, 'interHousehold Final'] ~ as.numeric(rownames(data2)))


#now, let's repeat - COPIED and PASTED from previous lines w/ our all-interpolated data2
vars[1] = 'interNet'
holdRows = rownames(data2)
#recode everything to numeric and NAs
data2 = apply(data2, 2, as.numeric)

#let's make the rownames something better
rownames(data2) = as.numeric(as.factor(holdRows)) + 1959

#wildly different scales, and maybe a problem with bimodality in outcome
#rescale
holdRows = rownames(data2)
data2 = apply(data2, 2, scale)
rownames(data2) = holdRows
#ignore time, just check bivariate correlations
#Linear regression - net mig as explanatory var, other vars as outcome vars - AGAIN w/interpolated vars
for(var in vars[-1]) assign(paste0('mod', var), lm(data2[, vars[1]] ~ data2[, var] -1))        

#store it all in one table and call it 'biLMtab'
biLMtab = matrix(NA, nrow = length(vars) - 1, ncol = 4)
for(i in 1:length(vars[-1])) biLMtab[i, ] = summary(get(paste0('mod', vars[i + 1])))$coef
rownames(biLMtab) = data['Series Name', vars[-1]]
colnames(biLMtab) = colnames(summary(get(paste0('mod', vars[i + 1])))$coef)

#Gaussian Process (1) to make inferences on 'interGDPGrowth ~ interNet'
data2 = as.data.frame(data2)  #first run data2
data3 = na.omit(data.frame('interGDPGrowth' = data2$interGDPGrowth, 'interNet' = data2$interNet))
data3$time = rownames(data3)
library(kernlab)
#Introduce time as explanatory
modGP = gausspr(interGDPGrowth ~ interNet + time,
            data = data3,
            kernel = 'rbfdot',
            type = 'regression',
            variance.model = T)
#Now plot to make inferences
meanX = data.frame('interNet' = mean(data3$interNet), 'time' = data3$time)
#y predictions
yPreds = predict(modGP, meanX)
yPredsLower = yPreds - 1.96*predict(modGP,
                                    meanX,
                                    type = 'sdeviation')
yPredsUpper = yPreds + 1.96*predict(modGP,
                                    meanX,
                                    type = 'sdeviation')
#standard deviation x
sdX = data.frame('interNet' = mean(data3$interNet - sd(data3$interNet)), 'time' = data3$time)
yPreds2 = predict(modGP, sdX)
yPredsLower2 = yPreds - 1.96*predict(modGP,
                                     sdX,
                                     type = 'sdeviation')
yPredsUpper2 = yPreds + 1.96*predict(modGP,
                                     sdX,
                                     type = 'sdeviation')
#now plot them
par(mfrow = c(1,1)) #1x1 dimensional on one plot
plot(yPreds ~ as.numeric(rownames(data3)), type = 'l', col = 'red', ylim = c(-2, 2.5))
lines(as.numeric(rownames(data3)), yPredsUpper, col = 'red', lty = 2) # lty makes it dashed
lines(as.numeric(rownames(data3)), yPredsLower, col = 'red', lty = 2)
lines(as.numeric(rownames(data3)), yPreds2, col = 'green', lty = 2)
lines(as.numeric(rownames(data3)), yPredsUpper2, col = 'green', lty = 2)
lines(as.numeric(rownames(data3)), yPredsLower2, col = 'green', lty = 2)

#Can repeat it the GP on other 3 outcome vars - seperately


#Lag the explanatory var - interNet - amended (correct version)
L1 = c()
for(row in 2:nrow(data2)){
  L1 = c(L1, data2[row - 1, 'interNet'])
}
data2$L1[2:nrow(data2)] = L1
L1


#From now (for the FINAL DRAFT); (1) Add random intercept and (2) include other countries 
#(to add more data and later you can write on differences - maybe touch upon USA differently)

#First step: Grab data and vars-needed (same vars) for other countries (UK, France, Germany, Denmark, Sweden, Estonia, Poland, Russia, Turkey, India, China, Japan)
#Second step: Check on how to run it - day10tss in our folder day 10 - line 128
"library(lme4)
REMod = lmer(politychanget1 ~ nonviol + (1|location) = to see the random effect on, meaning country = this is the random intercept here,
             data = data.1)"
#Third step: (1) Coefficient plots or (2) Tables (maybe do this) at the end of the code (check Day6 for tables, 'stargazer may not work')






---

"# XXX - Run first difference model (no package) - Term = interNet, Response = other 4 vars XXX
Log_Casualty.Model.did <- lm(data2[, vars[1]] ~ data2[, var] -1)
summary(Log_Casualty.Model.did)"

#Here, DiD or change-point model stats (plots and tables) won't give you anything specific - ***But adding a random intercept may give you sth to compare and analyze writing 

## XXX - After I settle all wrong setting here: - XXXX (Don't do it now, since I don't look at one specific time point (like Trump 2016))
## I will continue on -hopefully- non-parametric DiD for my next deadline that I planned for myself January 6th-7th and I can send you the Final Draft
## A change-point model as you noted can be useful to analyze the each time segment indivudually
## and doing so enable to deal with abrupt changes/temporal variations and come up with an overall pic

## I have also found an article that I'd like to discover more which is on "A Bayesian Change Point Model for Historical Time Series Analysis"
# The link: https://rmgsc.cr.usgs.gov/outgoing/threshold_articles/Western_Kleykamp2004.pdf




              
                     







