
##################################
# Investment Case Study - Solution

# Install Packages

#install.packages("dplyr")
# install.packages("tidyr")
  library(dplyr)
  library(tidyr)
  
##################################

#Checkpoint 1 

#Read files into data-frame

  setwd("C:/Upgrad/Investment Case Study/input")
  companies <- read.delim("companies.txt", header= TRUE, sep='\t',stringsAsFactors = FALSE,quote = NULL, fill = TRUE, comment.char = "")
  rounds2 <- read.csv("rounds2.csv", header=TRUE,sep=",",  stringsAsFactors = FALSE,quote = NULL, fill = TRUE, comment.char = "")

#Convert pkey case to lower in both the datasets
  
  rounds2$company_permalink <- tolower(rounds2$company_permalink)
  companies$permalink <- tolower(companies$permalink)
  
# Checkpoint 1 Solution

# How many unique companies are present in rounds2?
  unique_co_rnds <- n_distinct(rounds2$company_permalink)
  cat("Number of unique companies present in rounds2 dataset is: ", unique_co_rnds)

# How many unique companies are present in companies?
  unique_co_cos <- n_distinct(companies$permalink)
  cat("Number of unique companies present in rounds2 dataset is: ", unique_co_cos)
  
# In the companies data frame, which column can be used as the unique key for each company? 
  cat("The unique key for company dataset is: ", colnames(companies)[1])
   
#Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N

  non_matching <- count(rounds2[is.na(match(rounds2$company_permalink,companies$permalink)),])
  cat("Number of non matching companies in the rounds2 and companies file is:", non_matching$n)

# Merge the two data frames 
# How many observations are present in master_frame?

  master_frame <- merge(companies,rounds2, by.x=c("permalink"),by.y=c("company_permalink") )
  cat("Number of observations in the merge file is:" , length(master_frame$permalink))
  
#DATA CLEANING for the two datasets
  
#Remove records in Country data set having values in the variables Category_List and Country_Code as NA or blanks
 companies = companies %>% filter(!is.na(category_list),!is.na(country_code)) %>% filter(category_list != "" , country_code != "") 
  
# Remove records in Rounds2 data set with Raised_Amount_USD <= Zero 
  rounds2 = rounds2 %>% filter(raised_amount_usd>0) 
 
# Merge the two data frames post data cleaning
  
  master_frame <- merge(companies,rounds2, by.x=c("permalink"),by.y=c("company_permalink") )

##################################
  
#Checkpoint 2

# Average funding amount of venture type

  ave_fund_category = rounds2 %>% na.omit %>% group_by(funding_round_type) %>% summarise(ave_fund= mean(raised_amount_usd))

  ave_fund_seed <- ave_fund_category %>% filter(funding_round_type=="seed")
  ave_fund_venture <- ave_fund_category %>% filter(funding_round_type=="venture")
  ave_fund_angel <- ave_fund_category %>% filter(funding_round_type=="angel")
  ave_fund_private_equity <- ave_fund_category %>% filter(funding_round_type=="private_equity")
  
# Convert these data frames into a list
  
  ave_fund_list = Reduce(function(x, y) merge(x, y, all=TRUE), list(ave_fund_seed, ave_fund_venture, ave_fund_angel, ave_fund_private_equity))
  
#Spark Fund wants to invest between 5 to 15 million USD per investment round.

  chosen_inv_type = ave_fund_list %>% filter(ave_fund >= 5000000 & ave_fund < 15000000)
  cat("The investment type between 5-15M USD that Spark Funds wants to invest on is:", chosen_inv_type$funding_round_type)  

##################################

#Checkpoint 3

# Idenitfy Top 9 countries having the highest total funding (across ALL sectors for the chosen investment type)
# For the chosen investment type, make a data frame named top9 

  top9 = master_frame %>% filter(funding_round_type==chosen_inv_type$funding_round_type) %>% group_by(country_code)  %>% summarise(sum=sum(raised_amount_usd, na.rm=T))  %>% arrange(desc(sum)) %>% top_n(9)

  # Hard code the Top 9 cuntry's official language as English or not.
  Is_English <- c("Y","N","Y","Y","Y","N","N","N","N")
  top9["Is_English"] <- Is_English
  top3Eng <- top9 %>% filter(Is_English=="Y") %>% head(3) %>% select(1)
 
  # The top 3 English speaking country
  cat("The top 3 English speaking country are :", top3Eng$country_code)

##################################

#Checkpoint 4 

# Extract the primary sector of each category list from the category_list column; 
# first string before the vertical bar will be considered the primary sector. 

  companies_psector = companies %>% separate(category_list,into=c("category_prime","category_sec"),sep = "\\|",extra="merge")

# Read mapping file
  mapping <- read.csv("mapping.csv", header=TRUE,sep=",",stringsAsFactors = FALSE, na.strings = c("", "NA"))

# DATA CLEANING of MAPPING.CSV 

# Omit all na and blanks
  mapping <- mapping[!sapply(mapping, function (z) all(is.na(z) | z == ""))]

# Ensure all col names are correct [String Operational Optional]
  mapping = setNames(mapping,c("primary_sector","Automative & Sports","Blanks","Cleantech & Semiconductors", "Entertainment", "Health","Manufacturing","News, Search and Messaging","Others","Social,Finance, Analytics, Advertising"))

# Convert wide to long

# Column Range to convert
  x="Automative & Sports"
  y="Social,Finance, Analytics, Advertising"
  mapping = mapping %>% gather(mainsector, my_val, x:y) %>% filter(!my_val==0) %>% filter(`primary_sector`!=0) %>% select(-3)
  
# Merge with company set, map prime sector with a main sector
  company_sector_map <- merge(x=companies_psector,y=mapping, by.x=c("category_prime"),by.y=c("primary_sector"),all.x=TRUE)

# Rearrange the company_sector_map dataset - drop unnecessary columns
  columns_to_keep <- c("permalink", "name", "country_code", "category_prime","mainsector")
  company_sector_map <- subset(company_sector_map,select = (columns_to_keep))
 

# DATA CLEANING: drop records where primary sector not matching with any main sector
  company_sector_map = company_sector_map %>% filter(mainsector != "NA") 
 
##################################
  
#Checkpoint 5 Begins

#Initial Merge with master_frame and company_sector_map
  
  master_sectorV1 <- merge(x=master_frame, y=company_sector_map, by.x=c("permalink"),by.y=c("permalink"),all.x=TRUE)
  
#DATA CLEANING: drop records where primary sector has no matching main sector.
  master_sectorV1 = master_sectorV1 %>% filter(mainsector != "NA") 
  
# Create three data frames D1, D2 and D3 for each of the three countries
# containing the observations of funding type FT falling within the 5-15 million USD range.
  
# Create a var for the chosen type of funding for Spark
  fund_type=chosen_inv_type$funding_round_type
  
# Create var for the top 3 English speaking country
  country1=top3Eng$country_code[1]
  country2=top3Eng$country_code[2]
  country3=top3Eng$country_code[3]
  
  # BEGIN
  # create dataframe D1, for the first country, chosen fund type, and range of fund amount [5M-15M USD]
  
  dataframe_D1 <- subset(master_sectorV1, master_sectorV1$funding_round_type == fund_type & master_sectorV1$raised_amount_usd>= 5000000 & master_sectorV1$raised_amount_usd <=15000000 & master_sectorV1$country_code.y==country1 )
  
  #Add two columns - total count and total amount for each main sector in a separate column

  #convert mainsector to a factor
  dataframe_D1$mainsector <- as.factor(dataframe_D1$mainsector)
  
  #obtain the summary and convert it into a dataframe
  my.summary <- summary(dataframe_D1$mainsector)
  
  #The total count of investments for each main sector
  df_sector_inv_count <- data.frame(mainsector=names(my.summary), total_count_inv=my.summary)
  
  #Total amount invested for each main sector
  df_sector_inv_sum <- aggregate(raised_amount_usd~mainsector, data=dataframe_D1,sum)
  
  #Change colname for merge purpose
  colnames(df_sector_inv_sum) <- c("mainsector","total_sum_inv")
  
  # Merge the two data frames with mainsector as common key
  dataframe_D1 <- merge(x=dataframe_D1, y=df_sector_inv_count, by= "mainsector")
  dataframe_D1 <- merge(x=dataframe_D1, y=df_sector_inv_sum, by= "mainsector")
  
  # END
  
  # Repeat this exercise #BEGIN-#END for the dataframe D2
   
  dataframe_D2 <- subset(master_sectorV1, master_sectorV1$funding_round_type == fund_type & master_sectorV1$raised_amount_usd>= 5000000 & master_sectorV1$raised_amount_usd <=15000000 & master_sectorV1$country_code.y==country2 )
  dataframe_D2$mainsector <- as.factor(dataframe_D2$mainsector)
  my.summary <- summary(dataframe_D2$mainsector)
  df_sector_inv_count <- data.frame(mainsector=names(my.summary), total_count_inv=my.summary)
  df_sector_inv_sum <- aggregate(raised_amount_usd~mainsector, data=dataframe_D2,sum)
  colnames(df_sector_inv_sum) <- c("mainsector","total_sum_inv")
  dataframe_D2 <- merge(x=dataframe_D2, y=df_sector_inv_count, by= "mainsector")
  dataframe_D2 <- merge(x=dataframe_D2, y=df_sector_inv_sum, by= "mainsector")
  
  # Repeat this exercise #BEGIN-#END for the dataframe D3
  dataframe_D3 <- subset(master_sectorV1, master_sectorV1$funding_round_type == fund_type & master_sectorV1$raised_amount_usd>= 5000000 & master_sectorV1$raised_amount_usd <=15000000 & master_sectorV1$country_code.y==country3 )
  dataframe_D3$mainsector <- as.factor(dataframe_D3$mainsector)
  my.summary <- summary(dataframe_D3$mainsector)
  df_sector_inv_count <- data.frame(mainsector=names(my.summary), total_count_inv=my.summary)
  df_sector_inv_sum <- aggregate(raised_amount_usd~mainsector, data=dataframe_D3,sum)
  colnames(df_sector_inv_sum) <- c("mainsector","total_sum_inv")
  dataframe_D3 <- merge(x=dataframe_D3, y=df_sector_inv_count, by= "mainsector")
  dataframe_D3 <- merge(x=dataframe_D3, y=df_sector_inv_sum, by= "mainsector")
  
  
# Solution to Table 5  

# Create a vector with the three countries
  
  Country_Name <- c("USA","GBR","IND")
   
#  1. Total number of investments (count)

  cat("Total number of investments in D1", sum(unique(dataframe_D1$total_count_inv)))
  cat("Total number of investments in D2", sum(unique(dataframe_D2$total_count_inv)))
  cat("Total number of investments in D3", sum(unique(dataframe_D3$total_count_inv)))

   
# 2. Total amount of investment (USD)
  
  cat("Total amount of investments in D1", sum(unique(dataframe_D1$total_sum_inv)))
  cat("Total number of investments in D2", sum(unique(dataframe_D2$total_sum_inv)))
  cat("Total number of investments in D3", sum(unique(dataframe_D3$total_sum_inv)))
  
  
# 3. Top sector(based on count of investments)

  Top_count_Inv1 <- dataframe_D1 %>% count(mainsector) %>% filter(n==max(n))
  Top_count_Inv2 <- dataframe_D2 %>% count(mainsector) %>% filter(n==max(n))
  Top_count_Inv3 <- dataframe_D3 %>% count(mainsector) %>% filter(n==max(n))
  
  # 4. Second-best sector(based on count of investments)

  SecondBest_count_Inv1 <- dataframe_D1 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==2)
  SecondBest_count_Inv2 <- dataframe_D2 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==2)
  SecondBest_count_Inv3 <- dataframe_D3 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==2)
  
  
# 5. Third-best sector (based on count of investments)
  
  ThirdBest_count_Inv1 <- dataframe_D1 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==3)
  ThirdBest_count_Inv2 <- dataframe_D2 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==3)
  ThirdBest_count_Inv3 <- dataframe_D3 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==3)
  

# 6. Number of investments in the top sector (refer to point 3)
 
  Top_sector_invCount1 <- dataframe_D1 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(n==max(n)) %>% select(2)
  Top_sector_invCount2 <- dataframe_D2 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(n==max(n)) %>% select(2)
  Top_sector_invCount3 <- dataframe_D3 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(n==max(n)) %>% select(2)
  
  
# 7. Number of investments in the second-best sector (refer to point 4)
  
  Second_sector_invCount1 <- dataframe_D1 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==2) %>% select(2)
  Second_sector_invCount2 <- dataframe_D2 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==2) %>% select(2)
  Second_sector_invCount3 <- dataframe_D3 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==2) %>% select(2)
  

# 8. Number of investments in the third-best sector (refer to point 5)
  
  Third_sector_invCount1 <- dataframe_D1 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==3) %>% select(2)
  Third_sector_invCount2 <- dataframe_D2 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==3) %>% select(2)
  Third_sector_invCount3 <- dataframe_D3 %>% count(mainsector) %>% arrange(desc(n)) %>% filter(row_number()==3) %>% select(2)
  

# 9. For the top sector count-wise (point 3), which company received the highest investment?

  
   Comp_MaxInv_Sector_D1 <- dataframe_D1 %>% filter(mainsector=="Others") %>% group_by(name.x) %>% summarise(CompInv = sum(raised_amount_usd)) %>% arrange(desc(CompInv)) %>% filter(row_number()==1) %>% select(1)
   Comp_MaxInv_Sector_D2 <- dataframe_D2 %>% filter(mainsector=="Others") %>% group_by(name.x) %>% summarise(CompInv = sum(raised_amount_usd)) %>% arrange(desc(CompInv)) %>% filter(row_number()==1) %>% select(1)
   Comp_MaxInv_Sector_D3 <- dataframe_D3 %>% filter(mainsector=="Others") %>% group_by(name.x) %>% summarise(CompInv = sum(raised_amount_usd)) %>% arrange(desc(CompInv)) %>% filter(row_number()==1) %>% select(1)
  
# 10. For the second-best sector count-wise (point 4), which company received the highest investment?

  
   Comp_2ndInv_Sector_D1 <- dataframe_D1 %>% filter(mainsector=="Cleantech & Semiconductors") %>% group_by(name.x) %>% summarise(CompInv = sum(raised_amount_usd)) %>% arrange(desc(CompInv)) %>% filter(row_number()==1) %>% select(1)
   Comp_2ndInv_Sector_D2 <- dataframe_D2 %>% filter(mainsector=="Cleantech & Semiconductors") %>% group_by(name.x) %>% summarise(CompInv = sum(raised_amount_usd)) %>% arrange(desc(CompInv)) %>% filter(row_number()==1) %>% select(1)
   Comp_2ndInv_Sector_D3 <- dataframe_D3 %>% filter(mainsector=="News, Search and Messaging") %>% group_by(name.x) %>% summarise(CompInv = sum(raised_amount_usd)) %>% arrange(desc(CompInv)) %>% filter(row_number()==1) %>% select(1)
  

# Excel Analysis
   
   # setwd("C:/Upgrad/Investment Case Study/output")
   # write.csv(rounds2,file="rounds2.csv")
   # write.csv(companies,file="companies.csv")
   # write.csv(master_frame,file="master_frame.csv")
   # write.csv(master_sectorV1,file="master_sectorV1.csv")
   # write.csv(top9,file="top9.csv")
   # write.csv(dataframe_D1,file="dataframe_D1.csv")
   # write.csv(dataframe_D2,file="dataframe_D2.csv")
   # write.csv(dataframe_D3,file="dataframe_D3.csv")
  
  
  