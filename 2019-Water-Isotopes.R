# 2019 Gaspe Water Isotopes  #
# Kaiying Sally Ju | July 2020

## JD: Don't do this (Ben has reasons; you can accomplish it by restarting R)
## rm(list=ls())

library(dplyr)
library(ggplot2)
library(plyr)
library(tidyr)

## JD: Commands like this don't belong in code, which should run from beginning to end
## Comment them out, or type them in console
## JD: Again, don't do this.
## setwd("~/QMEE") # Home
df<-read.csv("./2019-Sept-WaterIsotopes.csv")

## JD: View is also not code
## View(df)
tibble(df)
str(df)

## JD: Some of this massaging would be nice to do using tables; we should come back to this
# 0. Massage Data #
df<-separate(df, "Sample", c("Site","Rep"), sep="919KKW") #Split Sample ID - unfortunately the easiest way removes 919
df$Sample<-c("919") # Sample=919 means that it was collected Aug 2019
df$Site<-as.factor(df$Site)
df$Sample<-as.factor(df$Sample)
spray.sites <- c("S5", "S6", "P20", "S3", "S1", "S2") #name the spray sites to add to treatment column
df$Treatment <- ifelse(df$Site %in% spray.sites, "Spray", "Defoliated") #Add treatment column
df$Site2 <- mapvalues(df$Site, from=c("S4","S5","S6","N4","N5","P20","N6","S3","N1","S1","N2","S2"), 
                      to=c("U01","U02","U03","C04","C05","C06","C07","L08","L09","L10","L11","L12")) #Add new site names (Site2)
df <- df %>%  # Add location of sites (U, C, L)
  separate(Site2, sep=1,into=c("Location"), extra="drop", remove=F) %>% # separate into location (U/C/L) based on site name
  filter(!grepl('QCD', Rep)) #Remove QC rows (these are QC replicates from the isotope lab)
# df <- df[c(9,10,8,7,2,3,4,5,6)] # Reorder columns
df$Location<-as.factor(df$Location)
df$Location<- factor(df$Location, levels = c("L", "C", "U")) # Order factors

df.clean<-df %>% #df.clean is a dataframe without the 3 samples that are contaminated 
  filter(!grepl('NBS', Contaminant)) 
tibble(head(df.clean, 20))

# 1. Explore the data #


## JD: Try to use spacing consistently; there's no reason why you keep indenting more after count. I would also wrap this in a print() so people can see why you did it.
df.clean %>% 
  group_by(Location) %>% 
  dplyr::summarise(
    count=n(),
      Maxd2H = max(d2H),
      Mind2H = min(d2H))

## JD: This is equivalent code
print(df.clean
	%>% group_by(Location)
	%>% dplyr::summarise(
		count=n(),
      Maxd2H = max(d2H),
      Mind2H = min(d2H)
	)
)

## Grade 1.9/3
