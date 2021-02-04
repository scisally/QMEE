# 2019 Gaspe Water Isotopes  #
# Kaiying Sally Ju | July 2020

library(tidyverse)

df<-read.csv("./2019-Sept-WaterIsotopes.csv")
## BMB: we like spaces around '<-' for readability
tibble(df) ## BMB: if you want to use tibbles anyway, you can use read_csv()
## instead of read.csv()
## why not just convert to a tibble? (this _views as_ as tibble but
##  doesn't save it)
str(df)
## BMB: note there are lots of blanks in Contaminant: is that on purpose?

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
S1,L10,Spray,L", header=TRUE, stringsAsFactors=TRUE)
## BMB: always use TRUE/FALSE instead of T/F

# 0. Massage Data #
df<-separate(df, "Sample", c("Site","Rep"), sep="919KKW") #Split Sample ID - unfortunately the easiest way removes 919
## BMB: it's too bad that there are no separators *and* that the terms differ in
## length. If the ID were always 3 digits ("N06", "P20" and the data were always four
## digites "0919", then separate() would let you separate by numerical position
df$Sample <- c("919") # Sample=919 means that it was collected Aug 2019
## BMB: this is a little dangerous, because it's hard-coded

## BMB: the weird ?= things are *lookahead* and
## lookbehind regular expressions: they split on 919 without removing it.
## this is still bad because we have the date hardcoded, and we can't make
## any general conclusions about where the split will be if we don't know
## how many digits are in the Sample ID vs the date ...
separate(df, "Sample", c("Site","tmp"), sep="(?=919)") %>%
    separate("tmp", c("date","Rep"), sep="(?<=919)")
    
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

## BMB: you could save() or saveRDS() what you had done up to this point
## and separate your scripts, using load() or readRDS() to get the clean
## data back into R ...

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

## BMB: how did you choose your binwidth?

df.clean %>%  # wrap by Site now
  ggplot(aes(d2H)) +
  geom_histogram(binwidth = 3, fill="lightblue", color="black") +
  ggtitle("Distribution of d2H Isotopes by different sites") +
  scale_y_continuous(breaks = seq(0, 11, by=1)) +
  facet_wrap(~Site)


## BMB: these are reasonable pictures, but it's not clear to me what you're
## looking for in terms of quality control of your data. I guess they're probably
## clean already (or you think they are), so you're not worried.  The graphical
## presentations _will_ help identify outliers etc.,
## although multivariate views will be more powerful than univariate views

## Grade: 2/3
