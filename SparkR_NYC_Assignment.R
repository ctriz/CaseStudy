# load SparkR
library(SparkR)

# initiating the spark session
sparkR.session(master='local')

# Read the data file

## 2015

data_nycpark <- read.df("s3://nycparking-1/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",
                        source = "csv", inferSchema = "true", header = "true")
## 2016

data_nycpark_16 <- read.df("s3://nycparking-1/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",
                           source = "csv", inferSchema = "true", header = "true")

## 2017

data_nycpark_17 <- read.df("s3://nycparking-1/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",
                           source = "csv", inferSchema = "true", header = "true")

# Examine data

## 2015

# Replace space with "_" for column
names(data_nycpark) <- gsub(" ","_",names(data_nycpark))

head(data_nycpark)
nrow(data_nycpark)
### Total no of rows : 11809233
ncol(data_nycpark)
### Total no of cols : 51
str(data_nycpark)
printSchema(data_nycpark)

# Describe the data and collect the described file

df <- collect(describe(data_nycpark))
df

## 2016
# Replace space with "_" for column
names(data_nycpark_16) <- gsub(" ","_",names(data_nycpark_16))

head(data_nycpark_16)
nrow(data_nycpark_16)
### Total no of rows : 10626899
ncol(data_nycpark_16)
### Total no of cols : 51
str(data_nycpark_16)
printSchema(data_nycpark_16)

# Describe the data and collect the described file

df_16 <- collect(describe(data_nycpark_16))
df_16

## 2017
# Replace space with "_" for column
names(data_nycpark_17) <- gsub(" ","_",names(data_nycpark_17))


head(data_nycpark_17)
nrow(data_nycpark_17)
### Total no of rows : 10803028
ncol(data_nycpark_17)
### Total no of cols : 43
str(data_nycpark_17)
printSchema(data_nycpark_17)

# Describe the data and collect the described file

df_17<- collect(describe(data_nycpark_17))
df_17

# Create a temporary view
## 2015
createOrReplaceTempView(data_nycpark, "data_nyc_tbl")


## 2016
createOrReplaceTempView(data_nycpark_16, "data_nyc_tbl_16")

## 2017
createOrReplaceTempView(data_nycpark_17, "data_nyc_tbl_17")


### 1) Find no. of tickets for each year

## 2015
data_tickets_2015 <- SparkR::sql("SELECT count(distinct(Summons_Number)) FROM data_nyc_tbl")
head(data_tickets_2015)

### Total no. of tickets for 2015 : 10951256

## 2016
data_tickets_2016 <- SparkR::sql("SELECT count(distinct(Summons_Number)) FROM data_nyc_tbl_16")
head(data_tickets_2016)

### Total no. of tickets for 2016 : 10626899

## 2017
data_tickets_2017 <- SparkR::sql("SELECT count(distinct(Summons_Number)) FROM data_nyc_tbl_17")
head(data_tickets_2017)

### Total no. of tickets for 2017 : 10803028

### 2) Find out how many unique states the cars which got parking tickets came from

## 2015
data_disc_state_2015 <- SparkR::sql("SELECT distinct(Registration_State) FROM data_nyc_tbl")
head(data_disc_state_2015)
nrow(data_disc_state_2015)


data_state_2015 <- SparkR::sql("SELECT count(distinct(Registration_State)) FROM data_nyc_tbl")
head(data_state_2015)

### No. of Unique state in 2015 : 69

## 2016

data_disc_state_2016 <- SparkR::sql("SELECT distinct(Registration_State) FROM data_nyc_tbl_16")
head(data_disc_state_2016)
nrow(data_disc_state_2016)


data_state_2016 <- SparkR::sql("SELECT count(distinct(Registration_State)) FROM data_nyc_tbl_16")
head(data_state_2016)


### No. of Unique state in 2016 : 68

## 2017

data_disc_state_2017 <- SparkR::sql("SELECT distinct(Registration_State) FROM data_nyc_tbl_17")
head(data_disc_state_2017)
nrow(data_disc_state_2017)

data_state_2017 <- SparkR::sql("SELECT count(distinct(Registration_State)) FROM data_nyc_tbl_17")
head(data_state_2017)


### No. of Unique state in 2017 : 67


### 3) Some parking tickets don't have addresses on them, which is cause for concern. Find out how many such tickets there are.

## 2015


data_address_2015 <- SparkR::sql("SELECT count(Street_Name) FROM data_nyc_tbl where Street_Name IS NULL or Street_Name =='' ")

head(data_address_2015)

### No. of parking tickets don't have addresses in 2015 : 0

## 2016
data_address_2016 <- SparkR::sql("SELECT count(Street_Name) FROM data_nyc_tbl_16 where Street_Name IS NULL or Street_Name =='' ")

head(data_address_2016)

### No. of parking tickets don't have addresses in 2016 : 0

## 2017
data_address_2017 <- SparkR::sql("SELECT count(Street_Name) FROM data_nyc_tbl_17 where Street_Name IS NULL or Street_Name =='' ")

head(data_address_2017)

### No. of parking tickets don't have addresses in 2017 : 0

# Aggregation tasks

### 1) How often does each violation code occur? (frequency of violation codes - find the top 5)

## 2015

data_violation_2015 <- SparkR::sql("SELECT Violation_Code, count(Summons_Number) as totalviolation FROM data_nyc_tbl GROUP BY Violation_Code ORDER BY totalviolation DESC")

head(data_violation_2015)

### Top 5 Violation_Code
#Violation_Code totalviolation
#1             21        1630912
#2             38        1418627
#3             14         988469
#4             36         839197
#5             37         795918
###

## 2016

data_violation_2016 <- SparkR::sql("SELECT Violation_Code, count(Summons_Number) as totalviolation FROM data_nyc_tbl_16 GROUP BY Violation_Code ORDER BY totalviolation DESC")

head(data_violation_2016)

### Top 5 Violation_Code
#Violation_Code totalviolation
#1             21        1531587
#2             36        1253512
#3             38        1143696
#4             14         875614
#5             37         686610


## 2017

data_violation_2017 <- SparkR::sql("SELECT Violation_Code, count(Summons_Number) as totalviolation FROM data_nyc_tbl_17 GROUP BY Violation_Code ORDER BY totalviolation DESC")

head(data_violation_2017)

### Top 5 Violation_Code
#Violation_Code totalviolation
#1             21        1528588
#2             36        1400614
#3             38        1062304
#4             14         893498
#5             20         618593

### 2) How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)

## 2015

data_vehiclebody_2015 <- SparkR::sql("SELECT Vehicle_Body_Type, count(Summons_Number) as totalvehicle FROM data_nyc_tbl GROUP BY Vehicle_Body_Type ORDER BY totalvehicle DESC")

head(data_vehiclebody_2015)

### Top 5 Vehicle_Body_Type
#Vehicle_Body_Type totalvehicle
#1              SUBN      3729346
#2              4DSD      3340014
#3               VAN      1709091
#4              DELV       892781
#5               SDN       524596

data_vehiclemake_2015 <- SparkR::sql("SELECT Vehicle_Make, count(Summons_Number) as totalvehicle FROM data_nyc_tbl GROUP BY Vehicle_Make ORDER BY totalvehicle DESC")

head(data_vehiclemake_2015)

### Top 5 Vehicle make
# Vehicle_Make totalvehicle
#1         FORD      1521874
#2        TOYOT      1217087
#3        HONDA      1102614
#4        NISSA       908783
#5        CHEVR       897845

## 2016

data_vehiclebody_2016 <- SparkR::sql("SELECT Vehicle_Body_Type, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 GROUP BY Vehicle_Body_Type ORDER BY totalvehicle DESC")

head(data_vehiclebody_2016)

### Top 5 Vehicle_Body_Type
#Vehicle_Body_Type totalvehicle
#1              SUBN      3466037
#2              4DSD      2992107
#3               VAN      1518303
#4              DELV       755282
#5               SDN       424043


data_vehiclemake_2016 <- SparkR::sql("SELECT Vehicle_Make, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 GROUP BY Vehicle_Make ORDER BY totalvehicle DESC")

head(data_vehiclemake_2016)

### Top 5 Vehicle make
# Vehicle_Make totalvehicle
#1         FORD      1324774
#2        TOYOT      1154790
#3        HONDA      1014074
#4        NISSA       834833
#5        CHEVR       759663

## 2017

data_vehiclebody_2017 <- SparkR::sql("SELECT Vehicle_Body_Type, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 GROUP BY Vehicle_Body_Type ORDER BY totalvehicle DESC")

head(data_vehiclebody_2017)

### Top 5 Vehicle_Body_Type
#Vehicle_Body_Type totalvehicle
#1              SUBN      3719802
#2              4DSD      3082020
#3               VAN      1411970
#4              DELV       687330
#5               SDN       438191

data_vehiclemake_2017 <- SparkR::sql("SELECT Vehicle_Make, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 GROUP BY Vehicle_Make ORDER BY totalvehicle DESC")

head(data_vehiclemake_2017)

### Top 5 Vehicle make
#Vehicle_Make totalvehicle
#1         FORD      1280958
#2        TOYOT      1211451
#3        HONDA      1079238
#4        NISSA       918590
#5        CHEVR       714655

### 3) A precinct is a police station that has a certain zone of the city under its command. Find the (5 highest) frequencies of:

## 2015
# Violating Precincts 

data_violating_2015 <- SparkR::sql("SELECT Violation_Precinct, count(Summons_Number) as totalvehicle FROM data_nyc_tbl GROUP BY Violation_Precinct ORDER BY totalvehicle DESC")

head(data_violating_2015)

### Top 5 Violation_Precinct
#Violation_Precinct totalvehicle
#1                  0      1799170
#2                 19       598351
#3                 18       427510
#4                 14       409064
#5                  1       329009

# Issuing Precincts

data_issuer_2015 <- SparkR::sql("SELECT Issuer_Precinct, count(Summons_Number) as totalvehicle FROM data_nyc_tbl GROUP BY Issuer_Precinct ORDER BY totalvehicle DESC")

head(data_issuer_2015)

### Top 5 Issuer_Precinct
#Issuer_Precinct totalvehicle
#1               0      2037745
#2              19       579998
#3              18       417329
#4              14       392922
#5               1       318778

## 2016
# Violating Precincts 

data_violating_2016 <- SparkR::sql("SELECT Violation_Precinct, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 GROUP BY Violation_Precinct ORDER BY totalvehicle DESC")

head(data_violating_2016)

### Top 5 Violation_Precinct
#Violation_Precinct totalvehicle
#1                  0      1868655
#2                 19       554465
#3                 18       331704
#4                 14       324467
#5                  1       303850


# Issuing Precincts

data_issuer_2016 <- SparkR::sql("SELECT Issuer_Precinct, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 GROUP BY Issuer_Precinct ORDER BY totalvehicle DESC")

head(data_issuer_2016)

### Top 5 Issuer_Precinct
#Issuer_Precinct totalvehicle
#1               0      2140274
#2              19       540569
#3              18       323132
#4              14       315311
#5               1       295013

## 2017
# Violating Precincts 

data_violating_2017 <- SparkR::sql("SELECT Violation_Precinct, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 GROUP BY Violation_Precinct ORDER BY totalvehicle DESC")

head(data_violating_2017)

### Top 5 Violation_Precinct
#Violation_Precinct totalvehicle

# Issuing Precincts
#1                  0      2072400
#2                 19       535671
#3                 14       352450
#4                  1       331810
#5                 18       306920

data_issuer_2017 <- SparkR::sql("SELECT Issuer_Precinct, count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 GROUP BY Issuer_Precinct ORDER BY totalvehicle DESC")

head(data_issuer_2017)

### Top 5 Issuer_Precinct
#1               0      2388479
#2              19       521513
#3              14       344977
#4               1       321170
#5              18       296553

### 4) Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

## 2015
data_violationprecincts0_2015 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl where Issuer_Precinct == 0 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts0_2015)

### Top 6 Violation_Code for Issuer_Precinct = 0
#Violation_Code Issuer_Precinct totalvehicle
#1             36               0       839197
#2              7               0       719745
#3              5               0       224516
#4             21               0       211975
#5             66               0         5597
#6             14               0         5016


data_violationprecincts19_2015 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl where Issuer_Precinct == 19 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts19_2015)

### Top 6 Violation_Code for Issuer_Precinct = 19
#Violation_Code Issuer_Precinct totalvehicle
#1             38              19        97154
#2             37              19        85007
#3             14              19        64133
#4             21              19        60215
#5             16              19        59675
#6             46              19        46363

data_violationprecincts18_2015 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl where Issuer_Precinct == 18 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts18_2015)

### Top 6 Violation_Code for Issuer_Precinct = 18
#Violation_Code Issuer_Precinct totalvehicle
#1             14              18       129079
#2             69              18        60618
#3             31              18        32925
#4             47              18        30872
#5             42              18        21026
#6             38              18        20013

# Highest frequency of violation code are not common for top 3 precinct

## 2016
data_violationprecincts0_2016 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 where Issuer_Precinct == 0 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts0_2016)

### Top 6 Violation_Code for Issuer_Precinct = 0
#Violation_Code Issuer_Precinct totalvehicle
#1             36               0      1253511
#2              7               0       492469
#3             21               0       237174
#4              5               0       112376
#5             66               0         7452
#6             14               0         5988

data_violationprecincts19_2016 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 where Issuer_Precinct == 19 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts19_2016)

### Top 6 Violation_Code for Issuer_Precinct = 19
#Violation_Code Issuer_Precinct totalvehicle
#1             38              19        77183
#2             37              19        75641
#3             46              19        73016
#4             14              19        61742
#5             21              19        58719
#6             16              19        52354

data_violationprecincts18_2016 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl_16 where Issuer_Precinct == 18 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts18_2016)

### Top 6 Violation_Code for Issuer_Precinct = 18
#Violation_Code Issuer_Precinct totalvehicle
#1             14              18        99857
#2             69              18        47881
#3             47              18        24009
#4             31              18        22809
#5             42              18        17678
#6             46              18        14674

# Highest frequency of violation code are not common for top 3 precinct

## 2017
data_violationprecincts0_2017 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 where Issuer_Precinct == 0 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts0_2017)

### Top 6 Violation_Code for Issuer_Precinct = 0
#Violation_Code Issuer_Precinct totalvehicle
#1             36               0      1400614
#2              7               0       516389
#3             21               0       268591
#4              5               0       145642
#5             66               0         9520
#6             14               0         7088

data_violationprecincts19_2017 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 where Issuer_Precinct == 19 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts19_2017)

### Top 6 Violation_Code for Issuer_Precinct = 19
#Violation_Code Issuer_Precinct totalvehicle
#1             46              19        86390
#2             37              19        72437
#3             38              19        72344
#4             14              19        57563
#5             21              19        54700
#6             16              19        31353

data_violationprecincts14_2017 <- SparkR::sql("SELECT Violation_Code,Issuer_Precinct,count(Summons_Number) as totalvehicle FROM data_nyc_tbl_17 where Issuer_Precinct == 18 GROUP BY Violation_Code,Issuer_Precinct  ORDER BY totalvehicle DESC")

head(data_violationprecincts14_2017)

### Top 6 Violation_Code for Issuer_Precinct = 14

#Violation_Code Issuer_Precinct totalvehicle
#1             14              18        91485
#2             69              18        36744
#3             47              18        23727
#4             31              18        21514
#5             46              18        14868
#6             42              18        12222
# Highest frequency of violation code are not common for top 3 precinct


#The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
#Find a way to deal with missing values, if any.
#Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. For each of these groups, find the 3 most commonly occurring violations


