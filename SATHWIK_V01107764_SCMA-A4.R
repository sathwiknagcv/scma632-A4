# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv("C:\\Users\\Admin\\Downloads\\Bootcamp Assignement\\A4\\Survey.csv",header=TRUE) 
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)


#A4-1. Do principal component analysis and factor analysis and identify the dimensions in the data. 

is.na(survey_df) 
sum(is.na(survey_df)) 
sur_int=survey_df[,20:46] 
str(sur_int) 
dim(sur_int) 
library(GPArotation) 
pca <- principal(sur_int,5,n.obs =162, rotate ="promax") 
pca 
om.h<-omega(sur_int,n.obs=162,sl=FALSE) 
op<-par(mfrow=c(1,1)) 
om<-omega(sur_int,n.obs=162) 
library(FactoMineR) 
pca <- princomp(sur_int, cor = TRUE)
summary(pca) 
biplot(pca, scale = 0) 
str(sur_int) 
dim(sur_int) 
show(sur_int) 

# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster","dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv('C:\\Users\\Admin\\Downloads\\Bootcamp Assignement\\A4\\Survey.csv',header=TRUE)
sur_int=survey_df[,19:46]

#Factor Analysis 
factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 


#A4.B. Carry our cluster analysis and characterize the respondents based on their background variables. 
library(cluster) 
library(factoextra) 
show(sur_int) 
fviz_nbclust(sur_int,kmeans,method = "gap_stat") 
set.seed(1234) 
km.res<-kmeans(sur_int,8,nstart = 25) 
fviz_cluster(km.res,data=sur_int,palette="jco", 
             ggtheme = theme_minimal()) 
res.hc <- hclust(dist(sur_int), method = "ward.D2") 
fviz_dend(res.hc,cex=0.5,k=8,palette = "jco") 
library(pheatmap) 
pheatmap(t(sur_int),cutree_cols = 8)

#A4.C. Do multidimensional scaling and interpret the results. 

icecream_df<-read.csv('C:\\Users\\Admin\\Downloads\\Bootcamp Assignement\\A4\\Icecream.csv',header=TRUE)
dim(icecream_df)

names(icecream_df) 

ice<-subset(icecream_df,select = -c(Brand)) 
distance_matrix<-dist(ice) 

mds_result<-cmdscale(distance_matrix,k=2) 

plot(mds_result[,1],mds_result[,2],pch=16,xlab="Dimension1",ylab="Dimension2",main="MDS plot") 

library(readr)
library(dplyr)
library(stats)
library(ggplot2)

#A4.4. Conjoint Analysis
# Load data
df <- read.csv('C:\\Users\\Admin\\Downloads\\Bootcamp Assignement\\A4\\pizza_data.csv')

# View the first few rows and shape of the data
head(df)
dim(df)

# Build the model
model <- 'ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy'
model_fit <- lm(model, data=df)
summary(model_fit)

# Define conjoint attributes
conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')

level_name <- list()
part_worth <- list()
part_worth_range <- c()
important_levels <- list()
end <- 1

for (item in conjoint_attributes) {
  nlevels <- length(unique(df[[item]]))
  level_name[[item]] <- unique(df[[item]])
  
  begin <- end
  end <- begin + nlevels - 1
  
  new_part_worth <- coef(model_fit)[begin:end]
  new_part_worth <- c(new_part_worth, (-1) * sum(new_part_worth))
  important_levels[[item]] <- which.max(new_part_worth)
  part_worth[[item]] <- new_part_worth
  part_worth_range <- c(part_worth_range, max(new_part_worth) - min(new_part_worth))
}

cat("-------------------------------------------------------------\n")
cat("level name:\n")
print(level_name)
cat("npw with sum element:\n")
print(new_part_worth)
cat("imp level:\n")
print(important_levels)
cat("part worth:\n")
print(part_worth)
cat("part_worth_range:\n")
print(part_worth_range)
cat("important levels:\n")
print(important_levels)


attribute_importance <- round(100 * part_worth_range / sum(part_worth_range), 2)
print(attribute_importance)

part_worth_dict <- list()
attrib_level <- list()
for (i in seq_along(conjoint_attributes)) {
  item <- conjoint_attributes[i]
  cat("Attribute :", item, "\n")
  cat("    Relative importance of attribute ", attribute_importance[i], "\n")
  cat("    Level wise part worths: \n")
  for (j in seq_along(level_name[[item]])) {
    cat("          ", level_name[[item]][j], ":", part_worth[[item]][j], "\n")
    part_worth_dict[[level_name[[item]][j]]] <- part_worth[[item]][j]
    attrib_level[[item]] <- level_name[[item]]
  }
}
print(part_worth_dict)

# Plot relative importance of attributes
importance_df <- data.frame(
  Attributes = conjoint_attributes,
  Importance = attribute_importance
)

ggplot(importance_df, aes(x=Attributes, y=Importance)) +
  geom_bar(stat='identity') +
  labs(title='Relative importance of attributes', x='Attributes', y='Importance')

# Calculate utility scores
utility <- numeric(nrow(df))
for (i in 1:nrow(df)) {
  score <- sum(
    part_worth_dict[[df$brand[i]]],
    part_worth_dict[[df$price[i]]],
    part_worth_dict[[df$weight[i]]],
    part_worth_dict[[df$crust[i]]],
    part_worth_dict[[df$cheese[i]]],
    part_worth_dict[[df$size[i]]],
    part_worth_dict[[df$toppings[i]]],
    part_worth_dict[[df$spicy[i]]]
  )
  utility[i] <- score
}

df$utility <- utility
print("The profile that has the highest utility score:\n")
print(df[which.max(utility),])

for (i in seq_along(attrib_level)) {
  cat("Preferred level in", names(attrib_level)[i], "is ::", attrib_level[[i]][important_levels[[i]]], "\n")
}




