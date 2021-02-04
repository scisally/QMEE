# 2019 Gaspe Water Isotopes  #
# Kaiying Sally Ju | July 2020

library(tidyverse)

df<-read.csv("./2019-Sept-WaterIsotopes.csv") 
tibble(df)
str(df)

siteinfo_table<-read.csv(text="
Site,Site2,Treatment,Location
N6,C07,Defoliated,C
N5,C05,Defoliated,C
N4,C04,Defoliated,C
P20,C06,Spray,C
N2,L11,Defoliated,L
N1,L09,Defoliated,L
S6,U03,Spray,U
S5,U02,Spray,U
S4,U01,Defoliated,U
S3,L08,Spray,L
S2,L12,Spray,L
S1,L10,Spray,L", header=T, stringsAsFactors=T)

# 0. Massage Data #
df<-separate(df, "Sample", c("Site","Rep"), sep="919KKW") #Split Sample ID - unfortunately the easiest way removes 919
# I'm not sure how to get the separated pieces to become factors.. 
df$Sample<-c("919") # Sample=919 means that it was collected Aug 2019
df$Site<-as.factor(df$Site)
df$Sample<-as.factor(df$Sample)
df.complete <- df %>% 
  left_join(siteinfo_table, by="Site")
df.clean <- df.complete %>% 
  filter(!grepl('QCD', Rep)) %>%  # Remove QC rows (these are QC replicates from the isotope lab)
  filter(!grepl('NBS', Contaminant)) %>% # Remove contaminated samples
  dplyr::rename(oldName=Site,Site=Site2) %>% 
  select(Site,oldName,Treatment,Location,Sample,Rep,d2H,d18O,d17O,Contaminant)

# 1. Explore the data #

## JD: Try to use spacing consistently; there's no reason why you keep indenting more after count. I would also wrap this in a print() so people can see why you did it.
print(df.clean %>%
  group_by(Location) %>%
  dplyr::summarise(
	  count=n(),
    Maxd2H = max(d2H),
    Mind2H = min(d2H), .groups="drop"))

## Grade 1.9/3

# 2. Data Exploration #
# Compare Spray and Defoliated sites #

str(df.clean)
summary(df.clean)

df.clean %>%  
  group_by(Location) %>% 
  ggplot(aes(x=Site, y=d2H, fill=Location)) + 
  geom_boxplot() +
  ggtitle("Water d2H in Spray vs Defoliated sites ")

# Distribution of d2H Isotopes across all sites 
df.clean %>% 
  ggplot(aes(d2H)) +
  geom_histogram(binwidth = 3, fill="lightblue", color="black") +
  ggtitle("Distribution of d2H Isotopes") +
  scale_y_continuous(breaks = seq(0, 11, by=1)) 

df.clean %>%  # wrap by Site now
  ggplot(aes(d2H)) +
  geom_histogram(binwidth = 3, fill="lightblue", color="black") +
  ggtitle("Distribution of d2H Isotopes by different sites") +
  scale_y_continuous(breaks = seq(0, 11, by=1)) +
  facet_wrap(~Site)


