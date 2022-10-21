install.packages("pastecs")
install.packages("corrplot")
install.packages("Amelia")

library(Amelia)
library(corrplot)
library(pastecs)
library(readr)

# read and import the dataset and convert it to a data frame
Life_Expectancy_Data1 <- read_csv("Life_Expectancy_Data1.csv")
LifeDF <- data.frame(Life_Expectancy_Data1)

# remove irrelevant observations/rows
LifeDF <- LifeDF[-c(218:266),]

# first 6 observation of the dataset
head(LifeDF)

# class of the whole dataset
class(LifeDF)

# dimension of the dataset row by column
dim(LifeDF)

# variable names of the dataset
colnames(LifeDF)

colnames(LifeDF) <- c("CountryName", "CountryCode", "Life_Expectancy(yrs)", "Electricity(%)", "National_Income(%)", 
                      "NIPerCapital(%)", "HIV_Children", "DropoutPrimary(%)", "Completed_Primary(%)", "Bachelors(%)", 
                      "Mortality_Rate(Per1k)", "Primary_Completion_Rate(%)", "Literacy_Rate(%)", "Interest_Rate(%)", 
                      "Population_Growth(%)", "Population_Density(per sq. km)", "Total_Population", "Current_Health_PerCapita(%)", 
                      "Current_Health_GDP(%)", "Unemployment(%)", "GDP_Growth(%)", "GDP_PerCapita($)", "Birth_Rate (Per1k)", 
                      "Energy_Consumption(%)", "HIV_Adult", "Water_Services(%)", "Poverty_Ratio(%)", "Compulsory_Education(yrs)")

# sapply(LifeDF, function(x) sum(is.na(x)))

# get the percentage of NAs for each variables
round(sapply (LifeDF, function(x) (sum(is.na(x))/nrow(LifeDF))*100), digits = 0)

# Map of missing vs observed values
missmap(LifeDF, main = "Missing values vs observed")

# list of variables to remove from the dataset
# drop <- c("Country.Name", "Country.Code", "SE.PRM.CUAT.ZS", "SE.TER.CUAT.BA.ZS", "SE.ADT.LITR.ZS", "SI.POV.LMIC", "EG.FEC.RNEW.ZS")
drop <- c("CountryName", "CountryCode", "Completed_Primary(%)", "Bachelors(%)", "Literacy_Rate(%)", 
          "Energy_Consumption(%)", "Poverty_Ratio(%)")

# drop variables
LifeDF = LifeDF[,!(names(LifeDF) %in% drop)]

# replace all .. with NA
LifeDF[LifeDF == ".."] <- NA

# convert Compulsory education, duration from character to numeric variable
LifeDF$`Compulsory_Education(yrs)` <- as.numeric(LifeDF$`Compulsory_Education(yrs)`)

# check the class of each variable
sapply(LifeDF, class)

# apply mean of each variable and replace with NAs
for(i in 1:ncol(LifeDF)){
  LifeDF[is.na(LifeDF[,i]), i] <- mean(LifeDF[,i], na.rm = TRUE)
}

### Lets discuss if we should round up the whole dataset
# LifeDF <- round(LifeDF, 0)

# round Compulsory education, duration to the nearest integer
LifeDF$`Compulsory_Education(yrs)` <- round(LifeDF$`Compulsory_Education(yrs)`, 0)

### question 1.(Descriptive statistics and numerical representations)

# give the statistical summary of all the variables in the dataset
stat.desc(LifeDF)

# get the min, lower-hinge, median, upper-hinge, & max % of population access to electricity
fivenum(LifeDF$`Electricity(%)`)

# maximum birthrate per 1,000 people
max(LifeDF$`Birth_Rate (Per1k)`)

# correlation matrix of the dataset
LifeDF.corr <- cor(LifeDF)

### Do some formatting on all plots e.g. changing the axis labels

# heatmap of the correlation matrix
corrplot(LifeDF.corr)

# scatter diagram of Current health expenditure per capita against Current health expenditure
plot(LifeDF$`Current_Health_PerCapita(%)`,LifeDF$`Current_Health_GDP(%)`)

# scatter diagram of access to electricity against total population
plot(LifeDF$`Electricity(%)`,LifeDF$Total_Population)

# Histogram of Current health expenditure per capita
hist(LifeDF$`Current_Health_PerCapita(%)`, breaks="FD", col="green")

# Histogram of Children out of primary school
hist(LifeDF$`DropoutPrimary(%)`,breaks = "FD", col = "yellow")

# scatter diagram of People using safely managed drinking water services against Population density
plot(LifeDF$`Water_Services(%)`,LifeDF$`Population_Density(per sq. km)`)

# maximum (\% of GDP) Current health expenditure
max(LifeDF$`Current_Health_GDP(%)`)

# maximum (current international \$) Current health expenditure per capita
max(LifeDF$`Current_Health_PerCapita(%)`)

# range of Primary completion rate, total (\% of relevant age group)
range(LifeDF$`Primary_Completion_Rate(%)`)

# summary of the dataset
summary(LifeDF)

round(cor(LifeDF), digits = 1)
round(LifeDF.corr, digits = 1)

reduced_Mod <- lm(LifeDF$`Life_Expectancy(yrs)` ~ LifeDF$`Electricity(%)`+ LifeDF$`Primary_Completion_Rate(%)` + LifeDF$`Current_Health_PerCapita(%)` 
             + LifeDF$`GDP_PerCapita($)` + LifeDF$`Water_Services(%)` + LifeDF$`Compulsory_Education(yrs)` + LifeDF$`Current_Health_GDP(%)` 
             + LifeDF$`Population_Density(per sq. km)`, data = LifeDF)
summary(reduced_Mod)

reduced_Mod2 <- lm(LifeDF$`Life_Expectancy(yrs)` ~ LifeDF$`Electricity(%)`+ LifeDF$`Compulsory_Education(yrs)` 
                  * LifeDF$`Population_Density(per sq. km)` + LifeDF$`Primary_Completion_Rate(%)` + LifeDF$`Current_Health_PerCapita(%)`
                  + LifeDF$`GDP_PerCapita($)` + LifeDF$`Water_Services(%)` + LifeDF$`Current_Health_GDP(%)`, data = LifeDF)
summary(reduced_Mod2)

vifs <- round(car::vif(reduced_Mod), digits = 2)
vifs
summary(vifs)

plot(vifs)

anova(reduced_Mod)
cor(LifeDF)
