##############################################################################
########### Library Repository Landscape Analysis #############################
########### Paula Schirrmacher ##############################################
##############################################################################

#load packages
library(tidyverse)# data wrangling and viz, includes ggplot2 and dplyr
library(reshape2)# change format of tables (melt and cast)
library(lubridate)#dates and times
library(data.table)#efficient and fast csv read and write
library(scales)#change axis scales in ggplot

#Dataframes will be manipulated and redefined throughout the script
#if you run out of memory, remove unused dfs with rm(dataframe)

#load and combine all files that start with Outputs_2020-23 and end with 0.csv (or 1,2,3)
#data should be located in same folder or subfolder of the R script (path=".")
#set column type for Year_of_Publication as factor ("f" - some odd ones make trouble), rest is set as best guess ("?")
items0<-list.files(path=".", pattern="^Outputs_2020-23.*0.csv$", recursive = T, full.names=TRUE) %>%
  lapply(read_csv, col_types = cols(.default = "?", Year_Of_Publication="f")) %>% 
  plyr::rbind.fill()%>%
  filter(Year_Of_Publication %in% c(2020,2021,2022))

items1<-list.files(path=".", pattern="^Outputs_2020-23.*1.csv$", recursive = T, full.names=TRUE) %>%
  lapply(read_csv, col_types = cols(.default = "?", Year_Of_Publication="f")) %>% 
  plyr::rbind.fill()%>%
  filter(Year_Of_Publication %in% c(2020,2021,2022))

items2<-list.files(path=".", pattern="^Outputs_2020-23.*2.csv$", recursive = T, full.names=TRUE) %>%
  lapply(read_csv, col_types = cols(.default = "?", Year_Of_Publication="f")) %>% 
  plyr::rbind.fill()%>%
  filter(Year_Of_Publication %in% c(2020,2021,2022))

items3<-list.files(path=".", pattern="^Outputs_2020-23.*3.csv$", recursive = T, full.names=TRUE) %>%
  lapply(read_csv, col_types = cols(.default = "?", Year_Of_Publication="f")) %>% 
  plyr::rbind.fill()%>%
  filter(Year_Of_Publication %in% c(2020,2021,2022))

#merge data from different years
#full join includes data from both tables, left and right
items<-items0%>%
  full_join(items1, by=c("Item", "Authors", "DOI", 
                         "Year_Of_Publication", "Metric_Type", "Item_Type", 
                         "Publisher", "Proprietary_ID", "Platform"),
            suffix=c("", "_2021"))%>% #join 2021 data to 2020 data and add suffix "_2021" to duplicate 2021 columns (Reporting_Period_Total) 
  full_join(items2, by=c("Item", "Authors", "DOI", 
                         "Year_Of_Publication", "Metric_Type", "Item_Type", 
                         "Publisher", "Proprietary_ID", "Platform"),
            suffix=c("", "_2022"))%>%#join 2022 data to 2020 and 2021 data and add suffix "_2022" to duplicate 2022 columns (Reporting_Period_Total)
  full_join(items3, by=c("Item", "Authors", "DOI", 
                         "Year_Of_Publication", "Metric_Type", "Item_Type", 
                         "Publisher", "Proprietary_ID", "Platform"),
            suffix=c("_2020","_2023"))#add 2023 data to rest; add suffix for first df (here 2020) in last join, otherwise names get muddled up

#reduce number of repositories based on number of outputs (here min 2000 outputs in 3 years)

repositories<-read.csv("repositories_names.csv")#all repositories

#compare like with like - filter for UK repos from universities
repos_uni_UK<-repositories%>%
  filter(organisation_country=="UK")%>% 
  filter(organisation_type %in% c("University", "University consortium"))%>%
  pull(platform)#extract vector of platforms for future filters

choose_platform<-items%>%
  filter(Platform %in% repos_uni_UK)%>% #filter for UK universities
  count(Platform)%>% #count outputs with non-zero downloads per platform (here: 1 row per output, so count rows of different Platform types)
  filter(n>2000)%>% #min 2000 outputs per platform
  arrange(desc(n)) #arrange df in descending order

subset_platforms<-choose_platform$Platform #vector of chosen Platforms for filtering

#reshape - melt data frame into long format
items_melted<-items%>%
  filter(Platform %in% subset_platforms)%>%#reduce size by filtering for chosen platforms
  mutate(Year_Of_Publication=factor(Year_Of_Publication))%>%#Year of Pub needs to be a factor or else it is also melted into long form
  select(-starts_with("URI"), -starts_with("Reporting"), -Metric_Type)%>%#remove columns that start with 'URI', 'Reporting' and column Metric_Type to tidy up
  reshape2::melt(variable.name="download_month", value.name ="downloads")#melt all numeric variables into long form

#add column with date, sort by date, add column with cumulative download count
#this step can take quite long - if R crashes delete unused dfs (e.g. rm(items)) and clear console (Ctrl + L) first
items_melted_date<-items_melted%>%
  mutate(Date= as.Date(paste(as.character(download_month), "-01", sep=""), 
                       format="%b-%Y-%d"))%>% #add decent date format to sort by (the 01 must be added because dates in R have to have day, month and year)
  group_by(Proprietary_ID)%>% #group data by output ID
  arrange(Date, by_group=TRUE)%>% #sort by Date within ID
  mutate(downloads_cumsum=cumsum(replace_na(downloads,0))) #add new var cumsum (cumulative sum), NAs are treated as 0

#narrow data down to 12th month
items_melted_reduced<-items_melted_date%>%
  group_by(Proprietary_ID)%>%
  arrange(Date, by_group=TRUE)%>% #sort by Date within each item
  mutate(downloads_log=ifelse(downloads_cumsum>=1,1,0))%>% #add new var that allows to filter by non-zero data
  filter(downloads_log==1)%>%#exclude all months before first download
  mutate(month_count = cumsum(downloads_log))%>%#add var for months from 1-12
  filter(month_count == 12)%>%#choose only the 12th month of download data
  select(-Date, -downloads, -downloads_log) #exclude columns that aren't needed anymore

#export data to csv
fwrite(items_melted_reduced, "full_list_outputs_12months.csv")

#identify download thresholds for top 10%, top50% and median by item type (for normalisation)
items_melted_reduced_quant<-items_melted_reduced%>%
  group_by(Item_Type)%>%
  summarise(top25perc_thresh=quantile(downloads_cumsum, probs=0.75),#75th percentile by item type
            top10perc_thresh=quantile(downloads_cumsum, probs=0.9),#90th percentile by item type
            top50perc_thresh=median(downloads_cumsum),#median download by item type (50th percentile)
            number_of_outputs=n())#number of outputs of this type across the whole dataset (all repos)

#apply above thresholds to data and evaluate for every output if it is in top 10, top 25 or top 50
items_melted_reduced_top<-items_melted_reduced%>%
  group_by(Item_Type)%>%
  mutate(top25perc_thresh=quantile(downloads_cumsum, probs=0.75),#add above summary column for item-type-normalised 75th percentile to df
         top10perc_thresh=quantile(downloads_cumsum, probs=0.9),#add above summary column for item-type-normalised 90th percentile to df
         top50perc_thresh=median(downloads_cumsum))%>%#add above summary column for item-type-normalised 50th percentile to df
  mutate(top25=ifelse(downloads_cumsum>=top25perc_thresh, 1, 0),#evaluate if output in item-type-normalised top 25 percent of downloads
         top10=ifelse(downloads_cumsum>=top10perc_thresh, 1, 0),#evaluate if output in item-type-normalised top 10 percent of downloads
         top50=ifelse(downloads_cumsum>=top50perc_thresh, 1, 0))#evaluate if output in item-type-normalised top50 percent of downloads (above avg)

items_melted_reduced_top_sum<-items_melted_reduced_top%>%
  group_by(Platform)%>%
  summarise(n_top25=sum(top25),#number of outputs in top 25% of all outputs (type-normalised) for each repository
            n_top10=sum(top10),#number of outputs in top 10% of all outputs (type-normalised) for each repository
            n_top50=sum(top50),#number of outputs in top 50% of all outputs (type-normalised) for each repository
            n=n(),#total number of outputs in repository
            prop_top10=n_top10/n, #proportion of outputs in overall top 10%
            prop_top25=n_top25/n, #proportion of outputs in overall top 25%
            prop_top50=n_top50/n) #proportion of outputs in overall top 50%

#export data to csv
fwrite(items_melted_reduced_top_sum, "prop_in_top10_top25_top50_norm_by_outputtype.csv")

#total number of outputs (n in items_melted_reduced_top_sum) shows that some repositories have less than 1000 outputs 
#(previous filter for min. 2000 outputs was on entire dataset (3 years), repositories with <1000 outputs here have majority of outputs in 2023, which were often excluded when data was reduced to outputs with 12months download)
subset_platforms1<-items_melted_reduced_top_sum%>%
  filter(n>1000)%>%
  pull(Platform)#reduce df to vector

#top 10, 25 and 50% metrics df needs reshaping for plotting to display data as stacked bar:
items_melted_reduced_top_sum1<-items_melted_reduced_top_sum%>%
  filter(Platform %in% subset_platforms1)%>%
  mutate(prop_top25_e=prop_top25 - prop_top10, #prop for top 25% --> top 25 excl. top 10 etc.
         prop_top50_e=prop_top50 - prop_top25, #prop for top 50% --> top 50 excl. top 25 etc.
         fill=1- prop_top50, #fill for everything below top 50%
         prop_top10_forsorting=prop_top10, #duplicate prop_top10 var to melt one into long form and keep one to sort data in plot
         check=ifelse(prop_top10+prop_top25_e+prop_top50_e+fill==1,1,0))#if 1, then it all adds up to 100%

#melt df into long form (as we need one var for color, one for x and one for y axis)
items_top_sum<-items_melted_reduced_top_sum1%>%
  select(Platform, prop_top10_forsorting, prop_top10, prop_top25_e, prop_top50_e, fill)%>%
  reshape2::melt(id.vars=c("Platform", "prop_top10_forsorting"), 
                 variable.name="prop_in_top_x", 
                 value.name ="prop")

#STACKED BARPLOT: Percent of outputs in top 10 (25,50) Percent of all outputs (output type normalised)
ggplot(items_top_sum, aes(x=reorder(Platform, prop_top10_forsorting), y=prop, 
                          fill=prop_in_top_x))+
  geom_col(position = position_stack(reverse = TRUE))+
  geom_hline(yintercept=0.1, linetype="dotted", col="grey70")+#add ref line for 10%
  geom_hline(yintercept=0.25, linetype="dotted", col="grey80")+ #add ref line for 25%
  geom_hline(yintercept=0.5, linetype="dotted", col="grey90")+ #add ref line for 50%
  scale_fill_manual(values=c("#FDE725FF", "#35B779FF", "#31688EFF","#440154FF"), 
                    labels = c("Top 10% of all", "Top 25% of all", "Top 50% of all", "Below average"))+#choose colors and color labels
  scale_y_continuous(labels = scales::percent, breaks=c(0.1,0.25, 0.5, 0.75))+ #y axis in percent with labels at 10%, 25%, 50% and 75%
  labs(x="", y="Number of Downloads", fill="Output type normalised\n percentiles")+
  coord_flip()+ #flip x and y axis (platform labels are easier to read)
  theme_classic(base_size = 12) # plot theme and increase overall font size

choose_output_types<-items_melted_reduced_quant%>%
  slice_max(n=10, order_by=number_of_outputs)

#What repos are comparable to WRRO and WREO by composition of output types and size?
#filter for top 10 output types to reduce size - otherwise the color legend is too big
#other output types have little effect
items_melted_reduced_type<-items_melted_reduced%>%
  filter(Item_Type %in% choose_output_types$Item_Type)%>%
  filter(Platform %in% subset_platforms1)%>%
  group_by(Platform)%>%
  mutate(n_total=n())%>%
  group_by(Platform, Item_Type, n_total)%>%
  summarise(n_Item=n())

#BAR PLOT (stacked bars absolute values)
ggplot(items_melted_reduced_type, aes(x=reorder(Platform, n_total), y=n_Item, fill=Item_Type))+
  geom_col()+
  scale_fill_brewer(palette="Paired")+#distinct color palette for 10 colours
  labs(x="", y="Number of outputs published and\ndownloaded at least once in 2020-2022", fill="")+
  coord_flip()+
  theme_classic(base_size=12)

#BAR PLOT (stacked bars stretched to 100% to compare composition)
ggplot(items_melted_reduced_type, aes(x=reorder(Platform, n_total), y=n_Item, fill=Item_Type))+
  geom_col(position="fill")+
  scale_fill_brewer(palette="Paired")+#distinct color palette for 10 colours
  theme_classic(base_size=12)+
  scale_y_continuous(labels = scales::percent)+
  labs(x="", y="Number of outputs published and\ndownloaded at least once in 2020-2022", fill="")+
  coord_flip()+
  theme_classic(base_size=12)

################################################################################
################STEP 2: DOWNLOADS BY COUNTRY ###################################
################################################################################


#download and bind all csv files that start with 'Repositories_type_country'
downloads<-list.files(path=".", pattern="^Repositories_type_country", recursive = T) %>%
  lapply(read_csv) %>% 
  plyr::rbind.fill() %>%
  filter(Metric_Type=='Unique_Item_Requests')#include only 'Unique_Item_Request' data

downloads$Platform<-as.factor(downloads$Platform)

WhiteRoseCountry<-subset(downloads, Platform=="White Rose Research Online")%>%
  group_by(Country)%>%#aggregate by Country
  summarise(total_downloads= sum(Reporting_Period_Total))%>%#sum of all downloads
  slice_max(n=12, order_by = total_downloads)#choose top 12 countries

#df of WREO download data by Country
WhiteRoseEthesisCountry<-subset(downloads, Platform=="White Rose Etheses Online")%>%
  group_by(Country)%>%#aggregate by Country
  summarise(total_downloads= sum(Reporting_Period_Total))%>% #sum of all downloads
  slice_max(n=12, order_by = total_downloads)#choose top 12 Countries

#average of all the Platforms (same UK repositories as in previous plots)
downloadsCountryThesis<-downloads%>% 
  group_by(Country)%>% #aggregate the data by country
  filter(Platform %in% subset_platforms)%>%
  filter(Item_Type=="Thesis or Dissertation")%>%
  summarise(total_downloads=sum(Reporting_Period_Total))%>% #sum of all downloads
  slice_max(n=12, order_by = total_downloads)#choose top 12 countries

downloadsCountryexclThesis<-downloads%>% 
  group_by(Country)%>% #aggregate the data by country
  filter(Platform %in% subset_platforms)%>%
  filter(Item_Type!="Thesis or Dissertation")%>%
  summarise(total_downloads=sum(Reporting_Period_Total))%>% #sum of all downloads
  slice_max(n=12, order_by = total_downloads)#choose top 12 countries

WhiteRoseCountry$Platform<-"White Rose Research Online"#add column called Platform
WhiteRoseEthesisCountry$Platform<-"White Rose Etheses Online"#add column called Platform
downloadsCountryThesis$Platform<-"All theses from repositories combined"#add column called Platform
downloadsCountryexclThesis$Platform<-"All repositories combined - excl. theses"#add column called Platform
downloadsCountry1<-rbind(downloadsCountryThesis, downloadsCountryexclThesis, WhiteRoseCountry, WhiteRoseEthesisCountry)#bind rows of all three dfs

#Plot Country data as stacked bar plot (stretched to 100%)
ggplot(downloadsCountry1, 
       aes(x=reorder(unlist(Platform), total_downloads), y=total_downloads, fill=reorder(Country,total_downloads)))+
  geom_col(position="fill")+#bar plot stretched to 100%
  scale_fill_manual(values=c('#fffac8','#ffe119', '#f58231','#e6194b','#800000', '#911eb4','#f032e6','#e6beff','#46f0f0',
                             '#4363d8','#000075','#008080','#808000','#3cb44b','#A1E8A1'))+
  labs(x="", y="Number of Downloads", fill="Top 12\ndownload\norigins")+#axis labels
  scale_y_continuous(labels = scales::percent)+#y axis in percent
  scale_x_discrete(limits=c("White Rose Etheses Online", "All theses from repositories combined",
                            "White Rose Research Online", "All repositories combined - excl. theses"))+
  theme_classic(base_size=12)+#apply classic plot theme and increase font size
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))#turn platform axis labels

