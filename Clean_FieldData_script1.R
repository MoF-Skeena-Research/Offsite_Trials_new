getwd()
Malek<- read.delim("C:/Users/MHAGHSHE/OneDrive - Government of BC/OffSite-Trials/Analysis/clean_data.txt")
head(Malek)



# Script to read and clean field data from 2022

# Load packages (install them if needed)
# if missing packages below, install them with the following code:
# install.packages("sf")

library(tidyverse) # bunch of useful packages
library(here) # good to use 
library(readxl) # required to read excel files
library(mapview) # use to quickly map sites
library(sf) # working with spatial data
library(bcmaps) # spatial datasets for BC, including BEC
EmpSal <- read.csv("C:/Users/MHAGHSHE/OneDrive - Government of BC/OffSite-Trials/Analysis/clean_data2.csv")
head(EmpSal)
# Read field data from 2022
x_raw <- 
  read_excel(here("C:/Users/MHAGHSHE/OneDrive - Government of BC/OffSite-Trials/Analysis/clean_final.xlsx"), sheet = "Fixed_clean_data")
head(x_raw)
# Load bec data (this can take awhile first time)

bgc<-bec()

# Start cleaning data
x_clean<-
  
  # start with raw data, and
  x_raw %>% 
  
  # Remove whitespace,new lines and return characters from all columns
  mutate(across(everything(),trimws)) %>%   
  
  # Replace whitespace in column names with underscore (easier to work with)
  rename_with(~str_replace_all(.x,pattern=" ",replacement="_")) %>% 
  
  # Rename some columns to make them shorter (my preference)
  rename(Spp=Spp_Code,
         Spp_loc=Local_or_Offsite,
         Vigour_var=Vigour_Variability,
         Rep=Replicate) %>% 
  
  # Standardize species codes
  mutate(Spp=str_to_title(Spp)) %>% 
  mutate(Spp=recode(Spp,Red_spruce="Sr", 
                    Fd="Fdi",
                    Pl="Pli")) %>%  # because this project will be provincial, we should specify if tree is interior or coastal variety (for Fd and Pl)
  
  # Convert columns to appropriate data types
  mutate(across(c(Latitude:Slope,Planting_Year,BHAge:NodeHeight4),as.numeric)) %>% # warning message the NAs introduced - need to double check Excel file
  mutate(across(c(Trial_type:Rep,BGC:Spp,Spp_loc,Damage:Vigour_var),factor)) %>%  # convert categorical variables to factors (R term for categorical variable)
  
  # Convert to spatial data
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326) %>%    # specify the crs for WGS1984 (datum used by Avenza to record coordinates, should double check)
  
  # Determine BEC for each site (notice it's missing for some of them)
  # This takes a few seconds to complete, so maybe comment this out or separate into different chunk
  transform_bc_albers() %>% # have to transform crs to bc albers
  st_join(bgc[,"MAP_LABEL"]) %>%  # this takes a few seconds
  rename(BGC_update2022=MAP_LABEL) %>% # rename column
  mutate(across(BGC_update2022,factor)) %>% # change to categorical variable
  relocate(BGC_update2022,.after=BGC) %>%  # move location so that can inspect (have to stop the pipe here to do so)
  
  # Note that the following code only updates missing BGC values from bcmaps.  BGC recorded in the field are kept.  This means that if someone
  # incorrectly identified BGC in the field, the error will still be in the data.
  mutate(BGC=coalesce(BGC,BGC_update2022)) %>%  # use the BEC data from bcmaps package to replace NA values in 'BGC' column
  dplyr::select(-BGC_update2022) # can remove this column now

# Visualize sites on map
mapview(x_clean,zcol="Spp")

# Start to explore data to detect any outliers or entry errors
levels(x_clean$Spp) # what is "Pg"?
boxplot(x_clean$DBH) # doesn't look like any outliers, good
boxplot(x_clean$Best_Height)
#The bec() function in R can fix them all, but not automatically.  You have to provide instructions.
#These are the instructions in the R code:
  # Convert to spatial data
  st_as_sf(coords=c("Longitude","Latitude",crs=4326) %>%    
  # specify the crs for WGS1984 (datum used by Avenza to record coordinates, should double check)
    # Determine BEC for each site (notice it's missing for some of them)
  # This takes a few seconds to complete, so maybe comment this out or separate into different chunk
  transform_bc_albers() %>% # have to transform crs to bc albers
  st_join(bgc[,"MAP_LABEL"])
  
  st_as_sf(coords=c("Longitude","Latitude",crs=4326)
          
x_clean$Spp[x_clean$Spp == 'Pg'] <- 'Bg'   #This code is to replace any word with desired one
#These codes are used to remove part of the word in a string 
x_clean$Survival_est[x_clean$Survival_est == 'Good_fair'] <- 'Good'
           
x_clean$Survival_est[x_clean$Survival_est == 'Excellent_good'] <- 'Excellent'

x_clean$Survival_est[x_clean$Survival_est == 'Fair_poor'] <- 'Fair'

x_clean$Survival_est[x_clean$Survival_est == 'Good_Fair'] <- 'Good'

x_clean$Survival_est[x_clean$Survival_est == 'Exellent_good'] <- 'Excellent'

x_clean$Survival_est[x_clean$Survival_est == 'Poor_fail'] <- 'Poor'

install.packages("weathercan")

transform_bc_albers() %>%
  
st_join(bgc[,"MAP_LABEL"]) 

library(bcmaps)
library(weathercan)
library(dplyr)
library(magrittr)
library(ggplot2)

unique(x_clean$BGC)

# This code is to create scatter plot based on BHAge and Ht
             
p <- ggplot(data = x_clean, aes(x = BHAge, y = Best_Height))
p + geom_point(aes(color = Spp_loc, shape = Spp_loc)) 

# to remove empty rows in a column
df_clean <- na.omit(x_clean['BGC'])

# To filter a column in dataframe and ctreate its own scatterplot
df_subset <- x_clean[x_clean$BGC %in% c("ICHvc"), ]
p <- ggplot(data = df_subset, aes(x = BHAge, y = Best_Height))
p + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "ICHvc sites")

df1_subset <- x_clean[x_clean$BGC %in% c("IDFdk3"), ]
p1 <- ggplot(data = df1_subset, aes(x = BHAge, y = Best_Height))
p1 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "IDFdk3 sites")

df2_subset <- x_clean[x_clean$BGC %in% c("ICHmc2"), ]
p2 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p2 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "ICHmc2 sites")

df3_subset <- x_clean[x_clean$BGC %in% c("SBPSdc"), ]
p3 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p3 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBPSdc sites")

df4_subset <- x_clean[x_clean$BGC %in% c("SBPSmk"), ]
p4 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p4 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBPSmk sites")

df5_subset <- x_clean[x_clean$BGC %in% c("SBSdk"), ]
p5 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p5 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSdk sites")

df6_subset <- x_clean[x_clean$BGC %in% c("SBSdw1"), ]
p6 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p6 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSdw1 sites")

df7_subset <- x_clean[x_clean$BGC %in% c("SBSdw3"), ]
p7 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p7 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSdw3 sites")

df8_subset <- x_clean[x_clean$BGC %in% c("SBSmc2"), ]
p8 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p8 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSmc2 sites")

df9_subset <- x_clean[x_clean$BGC %in% c("SBSmk1"), ]
p9 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p9 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSmk1 sites")

df10_subset <- x_clean[x_clean$BGC %in% c("SBSwk1"), ]
p10 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p10 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSwk1 sites")

df11_subset <- x_clean[x_clean$BGC %in% c("SBSwk2"), ]
p11 <- ggplot(data = df2_subset, aes(x = BHAge, y = Best_Height))
p11 + geom_point(aes(color = Spp, shape = Spp)) + labs(title = "SBSwk2 sites")

# Two ways to filter a specific coulumn in a data frame
Survive_clean <- na.omit(x_clean['Survival_est'])

Survival_est_clean <- subset(x_clean, !is.na(Survival_est) & Survival_est != "")

# To arrange data with color map in a scatter plot
ColorMap <- Survival_est_clean(x = BHAge, y = Best_Height, Survival_est = sample(c("Excellent", "Good", "Fair", "Poor", "Failed"), 50, replace = TRUE))
quality_colors <- c("Excellent" = "darkgreen", "Good" = "green", "Fair" = "lightgreen", "Poor" = "Yellow", "Failed" = "Orange")

ggplot(Survival_est_clean, aes(BHAge, Best_Height, color = Survival_est)) + 
  geom_point(size = 5) + 
  scale_color_manual(values = quality_colors, breaks = c("Excellent", "Good", "Fair", "Poor", "Failed"))
            