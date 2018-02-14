# This script is an automated data checker for Notre Dame PLS data.  It should be run on
# completed townships by exporting that township from Qualtrics (previously MySQL) to a csv.  
# The data checker will flag logical data entry errors for further investigation by humans.


##### Version 1.7 updates ####
#We switched from entering data with our GLO webform and storing the data in MySQL 
#to entering and storing data with qualtrics.nd.edu Project GLO_v1.0 and GLO_v2.0.
#Jody updated the code to be able to take the checked Qualtrics entries saved as csvs and
#run these double checks
# TO SEE THE FULL LIST OF VERSION UPDATES, GO TO THE BOTTOM OF THIS FILE


##  SET WORKING DIRECTORY to tell R where to look for the csv files.  Only run the line below
#  that applies to the computer you are working on (or add your own).  The file path should
#  lead to the "Township Data Checks" folder on your computer.
#  setwd("C:/Dropbox/PalEON Lab Team")  #lab GIS computer
#  setwd("C:/Users/jmurray7/Dropbox/PalEON Lab Team")  # Jody's work computer
#  setwd("/Users/paleolab/Dropbox/PalEON Lab Team")  #lab MAC
#  setwd("C:/Users/paleolab/Dropbox/PalEON Lab Team") #middle comp and laptop 
#  setwd("C:/Users/paleolab/Dropbox/PalEON Lab Team") #laptop double screen comp



## START code here for each township ##


# REMOVE ANY VARIABLES STORED IN THE ENVIRONMENT (for running code on multiple townships during one session)
rm(list=ls(all=TRUE)) 



##  READ IN EXPORTED TOWNSHIP .csv file (see google site for instructions on creating
#  and saving this file). 

#  ## WILL NEED TO CHANGE FILE NAME FOR YOUR TOWNSHIP ##
town <- read.csv("./Qualtrics Checks By Reader/Checked Townships/33N13E2_QualtricsCleaned_Checked.csv", header = TRUE, sep=",", na.strings = "NULL", stringsAsFactors = FALSE)
township.name <- "33N13E2_Dekalb"

#this is to read in the Hamilton Test township
#town <- read.csv("./Township Data Checks/5S7E3_Hamilton_Checked_witherrors.csv", header = TRUE, sep=",", na.strings = "NULL", stringsAsFactors = FALSE)


## HEY WAIT!  DID YOU READ IN THE RIGHT TOWNSHIP FILE AND NAME ABOVE? ##

# insert version number into report
R.checker.version <- "Version 1.7"


#change the 1/4 section entries to (1/4) section
town$typecorner = gsub("1/4 Section","(1/4) Section",town$typecorner) #change 1/4 Section in type corner to (1/4) Section to be consistent with the rest of the PLS database
town$typecorner = gsub("1/4 section","(1/4) Section",town$typecorner) #change 1/4 section in type corner to (1/4) Section to be consistent with the rest of the PLS database

#change the NAs in no data to blank
town$nodata = gsub("NA","",town$nodata)

#############################
## TYPOS OR MISSING VALUES ##
#############################

# list unique values of identification variables.  These lists can then be evaluated for typos (will create
# 2 levels and be easy to spot), blank values (""), etc, to identify items to go back into 
# MySQL and adjust if needed
surveyor = unique(town$surveyor_name)
volume <- unique(town$volume)
year <- unique(town$year)
state <- unique(town$state)
county <- unique(town$hubtack_county)
baseline <- unique(town$baseline)
meridian <- unique(town$meridian)
townshipdir <- unique(town$townshipdir)
rangedir <- unique(town$rangedir)
township <- unique(town$township)
range <- unique(town$rangenum)
typecorner <-(unique(town$typecorner))


########################
##### LOGIC CHECKS #####
########################

## CHECK valid sectiona and sectionb combinations (including missing values)

# First change sectiona and sectionb into numeric values instead of characters
# You will get a warning message that NAs were introduced by coercion. This is okay.
town$sectiona <- as.numeric(as.character(town$sectiona))
town$sectionb <- as.numeric(as.character(town$sectionb))

# read in table of valid section combinations.  For each section 1-36 (column 1), this table
# provides the valid section corner matches
combo <- read.csv("./Township Data Checks/sectionCombos.csv", stringsAsFactors = FALSE)

#remove the rows in town that have NAs in sectiona (column 19) and sectionb (column 20) 
#(these removed rows have data for the S and E borders which are checked below at lines 290 and 689)
town2 = town[complete.cases(town$sectiona & town$sectionb),]
#town2 = town[complete.cases(town[,19:20]),] #THIS IS THE OLD WAY THAT I REMOVED ROWS WITH NA. 
#Redid the code so it referenced the column headers (sectiona and sectionb)

# create a vector for the subsequent 'for loop' to put data into
is.combo <- rep(NA,length(town2$sectionb))

# run loop to test if sectionb is a valid compliment to sectiona for each row
for(i in 1:length(town2$sectionb)) {
  is.combo[i]<-town2$sectionb[i] %in% combo[town2$sectiona[i],2:5]}

#get the position of the FALSE values where the sectiona and sectionb combos don't match
wrong.section = which(is.combo %in% FALSE)

#create a data frame with the sectiona/sectionb combinations that are incorrect
section.combo = data.frame(town2[wrong.section,]$sectiona,town2[wrong.section,]$sectionb,town2[wrong.section,]$cornerid,town2[wrong.section,]$entry_id)
colnames(section.combo)<- c("sectiona","sectiona","cornerid","entry_id") 
section.combo



## CHECK valid section and section corner ID
# This check will make sure that the number entered for Section A (B) is actually a SECTION CORNER that could be 
# found for the entered corner ID.

###################
# CORNER SECTION A#
###################

# read in table of valid section and section corner ID combinations.  
#For each corner ID the table provides valid section number combinations.
combo1 <- read.csv("./Township Data Checks/SectionCornerIDs.csv")

# load dplr so that we can use left.join
library(dplyr)

# create new table that includes both combo1 information and town information. matched by cornerid.
matched1= left_join(combo1,town,by='cornerid')

#remove the corners that have NA in sectiona (column 24)
matched1b = matched1[complete.cases(matched1$sectiona),]
is.combo1 <- rep(NA,length(matched1b$sectiona))
for(i in 1:length(matched1b$sectiona)) {
  
  is.combo1[i]<-matched1b$sectiona[i] %in% matched1b[i,2:5]}

#find the entries that are marked FALSE and use those values in the next line of code
#to find those same row numbers in the matched1b data frame
wrong.sectionA = which(is.combo1 %in% FALSE)

#create a data frame with the sectiona/sectionb combinations that are incorrect
section.comboA = data.frame(matched1b[wrong.sectionA,]$sectiona,matched1b[wrong.sectionA,]$sectionb,matched1b[wrong.sectionA,]$cornerid,matched1b[wrong.sectionA,]$entry_id)
colnames(section.comboA)<- c("sectiona","sectionb","cornerid","entry_id") 
section.comboA

####################
# CORNER SECTION B #
####################

# read in table of valid section and section corner ID combinations.  
#For each corner ID the table provides valid section number combinations.
combo2 <- read.csv("./Township Data Checks/SectionCornerIDs.csv")

# create new table that includes both combo2 information and town information. matched by cornerid.
matched2= left_join(combo2,town,by='cornerid')

#remove the corners that have NA in sectionb (column 25)
matched2b = matched2[complete.cases(matched2$sectionb),]
is.combo2 <- rep(NA,length(matched2b$sectionb))
for(i in 1:length(matched2b$sectionb)) {
  
  is.combo2[i]<-matched2b$sectionb[i] %in% matched2b[i,2:5]}

#find the entries that are marked FALSE and use those values in the next line of code
#to find those same row numbers in the matched2b data frame
wrong.sectionB = which(is.combo2 %in% FALSE)

#create a data frame with the sectiona/sectionb combinations that are incorrect
section.comboB = data.frame(matched2b[wrong.sectionB,]$sectiona,matched2b[wrong.sectionB,]$sectionb,matched2b[wrong.sectionB,]$cornerid,matched2b[wrong.sectionB,]$entry_id)
colnames(section.comboB)<- c("sectiona","sectionb","cornerid","entry_id") 
section.comboB


#################
# 1/4 SECTION A #
#################

# CHECK valid section and quarter section corner ID
# This test is the same as above but will make sure the QUARTER SECTION CORNERID corresponds to the section numbers entered.  

# read in table of valid sections and quartersection ID combinations.  For each quartersection corner ID the table provides valid section number combinations.
combo3 <- read.csv("./Township Data Checks/QuarterSectionCornerIDs.csv")

# create new table that includes both combo3 information and town information. matched by cornerid.
matched3= left_join(combo3,town,by='cornerid')

#remove the corners that have NA in sectiona (column 22)
matched3b = matched3[complete.cases(matched3[ ,22]),]
is.combo3 <- rep(NA,length(matched3b$sectiona))
for(i in 1:length(matched3b$sectiona)) {
  
  is.combo3[i]<-matched3b$sectiona[i] %in% matched3b[i,2:3]}

#find the entries that are marked FALSE and use those values in the next line of code
#to find those same row numbers in the matched3b data frame
wrong.qsectionA = which(is.combo3 %in% FALSE)

#create a data frame with the sectiona/sectionb combinations that are incorrect
qsection.comboA = data.frame(matched3b[wrong.qsectionA,]$sectiona,matched3b[wrong.qsectionA,]$sectionb,matched3b[wrong.qsectionA,]$cornerid,matched3b[wrong.qsectionA,]$entry_id)
colnames(qsection.comboA)<- c("sectiona","sectionb","cornerid","entry_id") 
qsection.comboA



#################
# 1/4 SECTION B #
#################

# read in table of valid sections and quartersection ID combinations.  For each quartersection corner ID the table provides valid section number combinations.
combo4 <- read.csv("./Township Data Checks/QuarterSectionCornerIDs.csv")

# match quarter section corner ids and township file
matched4= left_join(combo4,town,by='cornerid')

#remove the corners that have NA in sectionb (column 23)
matched4b = matched4[complete.cases(matched4$sectionb),]
is.combo4 <- rep(NA,length(matched4b$sectionb))
for(i in 1:length(matched4b$sectionb)) {
  
  is.combo4[i]<-matched4b$sectionb[i] %in% matched4b[i,2:3]}

#find the entries that are marked FALSE and use those values in the next line of code
#to find those same row numbers in the matched3b data frame
wrong.qsectionB = which(is.combo4 %in% FALSE)

#create a data frame with the sectiona/sectionb combinations that are incorrect
qsection.comboB = data.frame(matched4b[wrong.qsectionB,]$sectiona,matched4b[wrong.qsectionB,]$sectionb,matched4b[wrong.qsectionB,]$cornerid,matched4b[wrong.qsectionB,]$entry_id)
colnames(qsection.comboB)<- c("sectiona","sectionb","cornerid","entry_id") 
qsection.comboB



## CHECK that there aren't entries for both sectiona, section b, and interior section.  
#Because interiorsection, interiordir, sectiona, and sectionb are all character values, the NAs are
#coming through as "NA" rather than NA.  Need to fix that first then can do the check to see
#if there are entries in both sectiona, sectionb and interior section

#change the "NA" characters in interiorsection & interiordir to NA so is.na() works
town$interiorsection[town$interiorsection=="NA"] <- NA
town$interiordir[town$interiordir=="NA"] <- NA


test = !(is.na(town$interiorsection)) & ( !(is.na(town$sectiona)) | !(is.na(town$sectionb)))
section.dup <- town[which(test == "TRUE"),c("sectiona","sectionb","interiorsection","interiordir","cornerid","entry_id")]
section.dup


######################################
# Interior Sections on S & E Borders #
######################################

## CHECK that all interiorsection section entries are valid section numbers touching border

# create vector of valid interior sections
int.section <- as.integer(c(1,2,3,4,5,6,7,18,19,30,31,32,33,34,35,36,25,24,13,12))


# check if interiorsection data entry is within possible values
test2 <- town$interiorsection %in% int.section
# filter out NA's in test (which represent non-border/interiorsections) and make report of invalid interiorsections
test3 <- test2 == FALSE & !is.na(town$interiorsection)
valid.interiorsection <- town[which(test3 == "TRUE"),c("interiorsection","interiordir","cornerid","entry_id")]
valid.interiorsection


#################
#Diameter Checks#  
#################

#convert each trees diameters from character to numeric
#when you do this there will be warnings that NAs were introduced by coercion. 
#that is good we want the blank values to be turned into NAs
town$diameter <- as.numeric(as.character(town$diameter))
town$diameter2 <- as.numeric(as.character(town$diameter2))
town$diameter3 <- as.numeric(as.character(town$diameter3))
town$diameter4 <- as.numeric(as.character(town$diameter4))


diameter.check = town[which(town$diameter >= 60 & town$diameter < 88888),c("cornerid","diameter","generalnotes","entry_id")]
diameter.check2 = town[which(town$diameter2 >= 60 & town$diameter2 < 88888),c("cornerid","diameter2","generalnotes","entry_id")]
diameter.check3 = town[which(town$diameter3 >= 60 & town$diameter3 < 88888),c("cornerid","diameter3","generalnotes","entry_id")]
diameter.check4 = town[which(town$diameter4 >= 60 & town$diameter4 < 88888),c("cornerid","diameter4","generalnotes","entry_id")]

#this will let the list of errors to come up in the final report below
diameter.errors <- list(diameter.check, diameter.check2, diameter.check3, diameter.check4)
diameter.errors


###################
#Bearing & Degrees# 
###################

# check that every bearing has a degrees value (and vice versa), bearing entries are N/S only, degrees are < 90
# We know that there will be trees on the E/W cardinal direction that this test will flag, but JP is okay 
# with having to check the handful of these that will come up.

#put NA in each tree's bearings that are blank
town$bearing[town$bearing==""] <- NA
town$bearing2[town$bearing2==""] <- NA
town$bearing3[town$bearing3==""] <- NA
town$bearing4[town$bearing4==""] <- NA

#convert each trees degrees from character to numeric
#when you do this there will be warnings that NAs were introduced by coercion. 
#that is good we want the blank values to be turned into NAs
town$degrees <- as.numeric(as.character(town$degrees))
town$degrees2 <- as.numeric(as.character(town$degrees2))
town$degrees3 <- as.numeric(as.character(town$degrees3))
town$degrees4 <- as.numeric(as.character(town$degrees4))


# test to see if the vector of true/falses match for:
# 1 - If degrees has value (True = value, False - NA/no data)
# 2 - if bearing has value (True = 1 of 4 possible values (N, S, 99999, 88888); False = no data/blank or invalid value)

## tree 1
match <- !is.na(town$degrees) == (town$bearing == "N" | town$bearing == "S" | town$bearing == "99999" | town$bearing == "88888")
match

# test to see if value of degrees is less than 90 (88888 and 99999 also valid)
acute <- town$degrees <= 90 | town$degrees == 99999 | town$degrees == 88888
acute

## repeat for tree 2
match2 <- !is.na(town$degrees2) == (town$bearing2 == "N" | town$bearing2 == "S" | town$bearing2 == "99999" | town$bearing2 == "88888")
acute2 <- town$degrees2 <= 90 | town$degrees2 == 99999 | town$degrees2 == 88888

## repeat for tree 3
match3 <- !is.na(town$degrees3) == (town$bearing3 == "N" | town$bearing3 == "S" | town$bearing3 == "99999" | town$bearing3 == "88888")
acute3 <- town$degrees3 <= 90 | town$degrees3 == 99999 | town$degrees3 == 88888

## repeat for tree 4
match4 <- !is.na(town$degrees4) == (town$bearing4 == "N" | town$bearing4 == "S" | town$bearing4 == "99999" | town$bearing4 == "88888")
acute4 <- town$degrees4 <= 90 | town$degrees4 == 99999 | town$degrees4 == 88888

#Get all the entries with bearings that are not N,S,88888,99999 and degrees that are >90
match.view <- town[which(match == "FALSE"),c("bearing", "degrees","bearingdir","chainstree","cornerid","entry_id")]
acute.view <- town[which(acute == "FALSE"),c("bearing", "degrees","bearingdir","chainstree","cornerid","entry_id")]
match.view2 <- town[which(match2 == "FALSE"),c("bearing2", "degrees2","bearingdir2","chainstree2","cornerid","entry_id")]
acute.view2 <- town[which(acute2 == "FALSE"),c("bearing2", "degrees2","bearingdir2","chainstree2","cornerid","entry_id")]
match.view3 <- town[which(match3 == "FALSE"),c("bearing3", "degrees3","bearingdir3","chainstree3","cornerid","entry_id")]
acute.view3 <- town[which(acute3 == "FALSE"),c("bearing3", "degrees3","bearingdir3","chainstree3","cornerid","entry_id")]
match.view4 <- town[which(match4 == "FALSE"),c("bearing4", "degrees4","bearingdir4","chainstree4","cornerid","entry_id")]
acute.view4 <- town[which(acute4 == "FALSE"),c("bearing4", "degrees4","bearingdir4","chainstree4","cornerid","entry_id")]


###########################
#Bearingdir and Chainstree#
###########################

# check that every bearingdir corresponds to a chainstree value (and vice versa), and bearingdir
# are always E and W

#put NA in each tree's bearingdirs that are blank
town$bearingdir[town$bearingdir==""] <- NA
town$bearingdir2[town$bearingdir2==""] <- NA
town$bearingdir3[town$bearingdir3==""] <- NA
town$bearingdir4[town$bearingdir4==""] <- NA

#convert each chainstrees degrees from character to numeric
#when you do this there will be warnings that NAs were introduced by coercion. 
#that is good we want the blank values to be turned into NAs
town$chainstree <- as.numeric(as.character(town$chainstree))
town$chainstree2 <- as.numeric(as.character(town$chainstree2))
town$chainstree3 <- as.numeric(as.character(town$chainstree3))
town$chainstree4 <- as.numeric(as.character(town$chainstree4))


# test to see if the vector of true/falses match for:
# 1 - If chainstree has value (True = value, False - NA/no data)
# 2 - if bearingdir has value (True = 1 of 4 possible values (N, S, 99999, 88888); False = no data/blank or invalid value)
compass <- !is.na(town$chainstree) == (town$bearingdir == "E" | town$bearingdir == "W" | town$bearingdir == "99999" | town$bearingdir == "88888")
compass.view <- town[which(compass == "FALSE"),c("bearing", "degrees","bearingdir","chainstree","cornerid","entry_id")]

## repeat for tree 2
compass2 <- !is.na(town$chainstree2) == (town$bearingdir2 == "E" | town$bearingdir2 == "W" | town$bearingdir2 == "99999" | town$bearingdir2 == "88888")
compass.view2 <- town[which(compass2 == "FALSE"),c("bearing2", "degrees2","bearingdir2","chainstree2","cornerid","entry_id")]

## repeat for tree 3
compass3 <- !is.na(town$chainstree3) == (town$bearingdir3 == "E" | town$bearingdir3 == "W" | town$bearingdir3 == "99999" | town$bearingdir3 == "88888")
compass.view3 <- town[which(compass3 == "FALSE"),c("bearing3", "degrees3","bearingdir3","chainstree3","cornerid","entry_id")]

## repeat for tree 4
compass4 <- !is.na(town$chainstree4) == (town$bearingdir4 == "E" | town$bearingdir4 == "W" | town$bearingdir4 == "99999" | town$bearingdir4 == "88888")
compass.view4 <- town[which(compass4 == "FALSE"),c("bearing4", "degrees4","bearingdir4","chainstree4","cornerid","entry_id")]


#this will let the list of errors to come up in the final report below
compass.interior <- list(compass.view, compass.view2, compass.view3, compass.view4)
compass.interior


########################
#Decimals in Chainstree#
########################

## CHECK for non-translated decimals in distance measurements (ie, 2.1 "links" should read 210 links, as it actually
# stands for 2.1 chains, and there are 100 links in each chain...metric-like surveyors, what what!)

# Sometimes there are fractions given with the chainstrees like "50 1/4 link"
# In this case the chainstree should be 50.25.  Common fractions seen in chainstrees are 1/4, 1/2, and 3/4
# 1/3 or 2/3 are also found but more rarely, so it is worth double checking the few entries that may come up for those
# the followin checks for decimal values other than 0.0, 0.25, 0.50 and 0.75 and return cornerid and entry id

#tree 1
chain1 <-data.frame(floor(town$chainstree))
chaindiff <-(town$chainstree-chain1)
newchain <-cbind(town, chaindiff)

# tree 2
chain2 <-data.frame(floor(town$chainstree2))
chaindiff2 <-(town$chainstree2-chain2)
newchain2 <-cbind(town, chaindiff2)

# tree 3
chain3 <-data.frame(floor(town$chainstree3))
chaindiff3 <-(town$chainstree3-chain3)
newchain3 <-cbind(town, chaindiff3)

# tree 4
chain4 <-data.frame(floor(town$chainstree4))
chaindiff4 <-(town$chainstree4-chain4)
newchain4 <-cbind(town, chaindiff4)

#Get all the entries with chainstrees that do not have decimals of 0.00, 0.25, 0.50, 0.75 or NA
#In the future, may want to add options for 0.33 or 0.66/0.67 for 1/3 or 2/3 fractions in the links
decimalstree1 <-newchain[which(newchain$floor.town.chainstree. != 0.50 & newchain$floor.town.chainstree. !=0.25 & newchain$floor.town.chainstree. !=0.00 & newchain$floor.town.chainstree. !=0.75 & !is.na(newchain$floor.town.chainstree.)),c("chainstree","cornerid","entry_id")]
decimalstree2 <-newchain2[which(newchain2$floor.town.chainstree2. != 0.50 & newchain2$floor.town.chainstree2. !=0.25 & newchain2$floor.town.chainstree2. !=0.00 & newchain2$floor.town.chainstree2. !=0.75 & !is.na(newchain2$floor.town.chainstree2.)),c("chainstree2","cornerid","entry_id")]
decimalstree3 <-newchain3[which(newchain3$floor.town.chainstree3. != 0.50 & newchain3$floor.town.chainstree3. !=0.25 & newchain3$floor.town.chainstree3. !=0.00 & newchain3$floor.town.chainstree3. !=0.75 & !is.na(newchain3$floor.town.chainstree3.)),c("chainstree3","cornerid","entry_id")]
decimalstree4 <-newchain4[which(newchain4$floor.town.chainstree4. != 0.50 & newchain4$floor.town.chainstree4. !=0.25 & newchain4$floor.town.chainstree4. !=0.00 & newchain4$floor.town.chainstree4. !=0.75 & !is.na(newchain4$floor.town.chainstree4.)),c("chainstree4","cornerid","entry_id")]

#this will let the list of errors to come up in the final report below
decimal.links <- data.frame(decimalstree1, decimalstree2, decimalstree3, decimalstree4)
decimal.links

###########################
# REFERENCE CORNER CHECKS #
###########################

# this just checks that the refcorner values are legitimate section corner numbers (i.e., not 1/4 section refcorner values)
# create vector of valid reference corner ids
reference.corners <- c(100100, 100200, 100300, 100400, 100500, 100600, 100700, 200100, 200200,
                       200300, 200400, 200500, 200600, 200700, 300100, 300200, 300300, 300400, 300500, 300600,
                       300700, 400100, 400200, 400300, 400400, 400500, 400600, 400700, 500100, 500200, 500300,
                       500400, 500500, 500600, 500700, 600100, 600200, 600300, 600400, 600500, 600600, 600700,
                       700100, 700200, 700300, 700400, 700500, 700600, 700700)

# Check for invalid values in "refcorner" or NAs
refcornertest = town[!(town$refcorner %in% reference.corners),c("refcorner","cornerid","entry_id")]
refcornertest


## REFERENCE CORNERS for SECTION CornerIDs 
# Check that the section cornerids have the right refcorner (cornerids can have multiple refcorners depending on direction
# of travel by the surveyor, but we want to make sure the refcorner is one of the right options. 
# For example, section corner 200100 can have refcorner 100100 or 300100, we want to make sure the database doesn't show 400100)


# read in table of valid reference corner and Section cornerID combinations.  For each section corner ID the table provides valid refcorner options.
section.refcombos <- read.csv("./Township Data Checks/sectionReferenceCorners.csv")

# match quarter section corner ids and township file
refcorner.match = left_join(section.refcombos,town,by='cornerid')

# Check if any of the refcorners for the section corners have NA in them. 
# If there are any rows with NA, this means that 1) these are 100100, 700100 or 700700 corners with no data,
# 2) there was no corner entered for that cornerid or 3) there is a corner but no refcorner was entered.
refcorner.NAs = refcorner.match[is.na(refcorner.match$refcorner),]


# remove the corners that have NA in refcorner 
refcorner.matchb = refcorner.match[complete.cases(refcorner.match$refcorner),]
is.combo.ref <- rep(NA,length(refcorner.matchb$refcorner))
for(i in 1:length(refcorner.matchb$refcorner)) {
  
  is.combo.ref[i]<-refcorner.matchb$refcorner[i] %in% refcorner.matchb[i,2:5]}

#find the entries that are marked FALSE and use those values in the next line of code
#to find those same row numbers in the refcorner.matchb data frame
wrong.section.refcorner = which(is.combo.ref %in% FALSE)

#create a data frame with the cornerid/refcorner combinations that are incorrect
section.combo.ref = data.frame(refcorner.matchb[wrong.section.refcorner,]$refcorner,refcorner.matchb[wrong.section.refcorner,]$cornerid,refcorner.matchb[wrong.section.refcorner,]$sectiona,refcorner.matchb[wrong.section.refcorner,]$sectionb,refcorner.matchb[wrong.section.refcorner,]$interiorsection,refcorner.matchb[wrong.section.refcorner,]$interiordir,refcorner.matchb[wrong.section.refcorner,]$entry_id)
colnames(section.combo.ref)<- c("refcorner","cornerid","sectiona","sectiona","interiorsection", "interiordir","entry_id") 
section.combo.ref



## REFERENCE CORNERS for (1/4) SECTION CornerIDs 
# Check that the quarter section cornerids have the right refcorner (cornerids can have multiple refcorners depending on direction
# of travel by the surveyor, but we want to make sure the refcorner is one of the right options. 
# For example, quarter section corner 500140 can have refcorner 500100 or 500200)


# read in table of valid reference corner and Section cornerID combinations.  For each section corner ID the table provides valid refcorner options.
qsection.refcombos <- read.csv("./Township Data Checks/QuarterSectionReferecenCorners.csv")

# match quarter section corner ids and township file
qrefcorner.match = left_join(qsection.refcombos,town,by='cornerid')

# Check if any of the refcorners for the (1/4) section corners have NA in them. 
# These NAs typically won't happen with a complete township.  When you do get NA it is because either
# 1) there was no corner entered for that cornerid or 2) there is a corner but no refcorner was entered.
qrefcorner.NAs = qrefcorner.match[is.na(qrefcorner.match$refcorner),]
qrefcorner.NAs

# remove the corners that have NA in refcorner 
# these NAs typically won't happen with a complete township. You will get an NA if a (1/4) section corner is missing
qrefcorner.matchb = qrefcorner.match[complete.cases(qrefcorner.match$refcorner),]
q_is.combo.ref <- rep(NA,length(qrefcorner.matchb$refcorner))
for(i in 1:length(qrefcorner.matchb$refcorner)) {
  
  q_is.combo.ref[i]<-qrefcorner.matchb$refcorner[i] %in% qrefcorner.matchb[i,2:3]}

#find the entries that are marked FALSE and use those values in the next line of code
#to find those same row numbers in the refcorner.matchb data frame
q_wrong.section.refcorner = which(q_is.combo.ref %in% FALSE)

#create a data frame with the cornerid/refcorner combinations that are incorrect
qsection.combo.ref = data.frame(qrefcorner.matchb[q_wrong.section.refcorner,]$refcorner,qrefcorner.matchb[q_wrong.section.refcorner,]$cornerid,qrefcorner.matchb[q_wrong.section.refcorner,]$sectiona,qrefcorner.matchb[q_wrong.section.refcorner,]$sectionb,qrefcorner.matchb[q_wrong.section.refcorner,]$interiorsection,qrefcorner.matchb[q_wrong.section.refcorner,]$interiordir,qrefcorner.matchb[q_wrong.section.refcorner,]$entry_id)
colnames(qsection.combo.ref)<- c("refcorner","cornerid","sectiona","sectiona","interiorsection", "interiordir","entry_id") 
qsection.combo.ref


######################
# DATA CORNER CHECKS #
######################

# This section checks that data was collected for each section corner and 1/4 section corner in the township.

## TYPECORNER checks
# Check to make sure that every typecorner labeled (1/4) Section, Section, or Township has a valid cornerid

##Section Check
section.corners <- c(100200,100300,100400,100500,100600,200100,200200,200300,200400,200500,200600,200700,
                     300100,300200,300300,300400,300500,300600,300700,400100,400200,400300,400400,400500,400600,400700,
                     500100,500200,500300,500400,500500,500600,500700,600100,600200,600300,600400,600500,600600,600700,
                     700200,700300,700400,700500,700600)


#make a dummy database w/ section.corners attached to letter "a"
section.cornersx=cbind(data.frame(section.corners),"a")
colnames(section.cornersx)=c("cornerid","who")

# subset township data to just section typecorner
section.rows <- town[town$typecorner == "Section",]

#Merging section.rows, and section.cornersx, by corner id. NOTE: The order of all.x and all.y matters (same w/ regards to true false)
#x=merge(section.rows,section.cornersx,by=c("cornerid"),all.x=T,all.y=F)
sectionmerge=merge(section.rows,section.cornersx,by=c("cornerid"),all.x=T,all.y=F)

#make a database w/ wrong cornerid from section.rows
#new_DF <- subset(x, is.na(x$who))
new_sectionmerge <- subset(sectionmerge, is.na(sectionmerge$who))

#creates new dataframe that binds cornerid, entry_id, and error message
#then creates a second new dataframe that lists NAs for cornerID and entryID
#then checks the dimensions of the first new dataframe - if it is too small (i.e., there are no errors), the output lists the 
#second NA dataframe. If if the first new dataframe has errors then that gets listed in the output
new_sectionmerge2=cbind(new_sectionmerge$cornerid,"Section","mismatched cornerid and section",new_sectionmerge$entry_id)
new_sectionmergeNA=cbind("NA", "Section", "mismatched cornerid and Section","NA")
if(dim(new_sectionmerge2)[2]<4){new_sectionmerge2=new_sectionmergeNA} else{new_sectionmerge2=new_sectionmerge2}
colnames(new_sectionmerge2)=c("cornerid","tyecorner","error","entry_id")
new_sectionmerge2

##Quarter Sections Check
quarter.sections <- c(100140,100240,100340,100440,100540,100640,140100,140200,140300,140400,140500,140600,
                      140700,200140,200240,200340,200440,200540,200640,240100,240200,240300,240400,240500,240600,240700,
                      300140,300240,300340,300440,300540,300640,340100,340200,340300,340400,340500,340600,340700,
                      400140,400240,400340,400440,400540,400640,440100,440200,440300,440400,440500,440600,440700,
                      500140,500240,500340,500440,500540,500640,540100,540200,540300,540400,540500,540600,540700,
                      600140,600240,600340,600440,600540,600640,640100,640200,640300,640400,640500,640600,640700,
                      700140,700240,700340,700440,700540,700640)


#make a dummy database w/ quarter.sections attached to letter "a"
quarter.sectionsx=cbind(data.frame(quarter.sections),"a")
colnames(quarter.sectionsx)=c("cornerid","who")


# subset the township data to just the quarter section typecorners
quarter.rows <- town[town$typecorner == "(1/4) Section",]

#Merging quarter.rows, and quarter.sectionsx, by corner id. NOTE: The order of all.x and all.y matters (same w/ regards to true false)
#y=merge(quarter.rows,quarter.sectionsx,by=c("cornerid"),all.x=T,all.y=F)
quartermerge=merge(quarter.rows,quarter.sectionsx,by=c("cornerid"),all.x=T,all.y=F)
#make a database w/ wrong cornerid from quarter.rows
#new_DFy <- subset(y, is.na(y$who))
new_quartermerge <- subset(quartermerge, is.na(quartermerge$who))

#creates new dataframe that binds cornerid, entry_id, and error message
#then creates a second new dataframe that lists NAs for cornerID and entryID
#then checks the dimensions of the first new dataframe - if it is too small (i.e., there are no errors), the output lists the 
#second NA dataframe. If if the first new dataframe has errors then that gets listed in the output
new_quartermerge2=cbind(new_quartermerge$cornerid,"(1/4) Section", "mismatched cornerid and (1/4) section",new_quartermerge$entry_id)
new_quartermergeNA=cbind("NA", "(1/4) Section", "mismatched cornerid and (1/4) Section","NA")
if(dim(new_quartermerge2)[2]<4){new_quartermerge2=new_quartermergeNA} else{new_quartermerge2=new_quartermerge2}
colnames(new_quartermerge2)=c("cornerid","typecorner","error","entry_id")
new_quartermerge2

##Township Corners Check

township.corners <- c(100100,700100,700700)

#make a dummy database w/ townships.sections attached to letter "a"
township.cornersx=cbind(data.frame(township.corners),"a")
colnames(township.cornersx)=c("cornerid","who")

# subset the township data to just the township section typecorners
township.rows <- town[town$typecorner == "Township",]

#Merging township.rows, and township.sectionsx, by corner id. NOTE: The order of all.x and all.y matters (same w/ regards to true false)
#y=merge(quarter.rows,quarter.sectionsx,by=c("cornerid"),all.x=T,all.y=F)
townshipmerge=merge(township.rows,township.cornersx,by=c("cornerid"),all.x=T,all.y=F)
#make a database w/ wrong cornerid from quarter.rows
#new_DFy <- subset(y, is.na(y$who))
new_townshipmerge <- subset(townshipmerge, is.na(townshipmerge$who))

#creates new dataframe that binds cornerid, entry_id, and error message
#then creates a second new dataframe that lists NAs for cornerID and entryID
#then checks the dimensions of the first new dataframe - if it is too small (i.e., there are no errors), the output lists the 
#second NA dataframe. If if the first new dataframe has errors then that gets listed in the output
new_townshipmerge2=cbind(new_townshipmerge$cornerid,"Township","mismatched cornerid and Township",new_townshipmerge$entry_id)
new_townshipmergeNA=cbind("NA", "Township","mismatched cornerid and Township","NA")
if(dim(new_townshipmerge2)[2]<4){new_townshipmerge2=new_townshipmergeNA} else{new_townshipmerge2=new_townshipmerge2}
colnames(new_townshipmerge2)=c("cornerid","typecorner","error","entry_id")
new_townshipmerge2

sectionCheck=rbind(new_sectionmerge2,new_quartermerge2, new_townshipmerge2)
sectionCheck


## Corner completeness check ##
# tests to see if there are cornerids missing from the internal corner and South and East border vectors below

# Create vectors of the Internal Corners and the South and East borders - we could probably combine these two vectors
# But for now I'm leaving them separate
# Internal Corners (non borders)#
internal.corners <- c(140200, 140300, 140400, 140500, 140600, 200140, 200200, 240200, 200240, 200300,
                      240300, 200340, 200400, 240400, 200440, 200500, 240500, 200540, 200600, 240600, 200640, 300140, 
                      300200, 340200, 300240, 300300, 340300, 300340, 300400, 340400, 300440, 300500, 340500, 300540, 
                      300600, 340600, 300640, 400140, 400200, 440200, 400240, 400300, 440300, 400340, 400400, 440400, 
                      400440, 400500, 440500, 400540, 400600, 440600, 400640, 500140, 500200, 540200, 500240, 500300, 
                      540300, 500340, 500400, 540400, 500440, 500500, 540500, 500540, 500600, 540600, 500640, 600140, 
                      600200, 640200, 600240, 600300, 640300, 600340, 600400, 640400, 600440, 600500, 640500, 600540, 
                      600600, 640600, 600640) 

# South and East borders #
SEborder.corners <- c(100100, 140100, 200100, 240100, 300100, 340100, 400100, 440100, 500100, 540100, 600100,
                      640100, 700100, 700140, 700200, 700240, 700300, 700340, 700400, 700440, 700500, 700540, 700600, 700640,700700)

# do the test to see if there are cornerids missing from the internal corner and SE border vectors
internalcorners.test = internal.corners[!(internal.corners %in% town$cornerid)]
SEbordercorners.test = SEborder.corners[!(SEborder.corners %in% town$cornerid)]

error = "cornerid not found in township data"

missing.corner = cbind(internalcorners.test,error)
missing.corner

missing.border = cbind(SEbordercorners.test,error)
missing.border

# North and West borders #
# Test to see if there are any North or West borders - there should not be!

# create a vector with north and west border corner id values
NWborder.corners <- c(100140, 100200, 100240, 100300, 100340, 100400, 100440, 100500, 100540, 100600, 100640,
                      100700, 140700, 200700, 240700, 300700, 340700, 400700, 440700, 500700, 540700, 600700, 640700)

NWbordercorners.test = NWborder.corners[which(NWborder.corners %in% town$cornerid == "TRUE")]
NWbordercorners.test

#######################
# Duplicate CornerIDs #
#######################

#check for any duplicated cornerIDs

cornerid.dups = town[duplicated(town$cornerid)|duplicated(town$cornerid, fromLast = TRUE),c("sectiona","sectionb","interiorsection","interiordir","refcorner","traveldir","secondpass","chains","typecorner","cornerid","entry_id")]
cornerid.dups = cornerid.dups[order(cornerid.dups$cornerid),] 
cornerid.dups

############################
## CREATE LIST OF RECHECKS##
############################

# create list of all entries flagged for recheck with reason and any fixed messages

# if flagged for recheck, list error and fixed message, if present
recheck = town[which(town$recheck == "Y"),c("cornerid","recheck","reason","entry_id")]
recheck


## LIST ERRORS FOR OUTPUT ##
# create report metadata
R.Check.Date = format(Sys.time(), " %b %d %Y")
Name  <- levels(town$reader_initials)
prime.mer <- levels(as.factor(town$meridian))

metadata <- rbind(Name, township.name, prime.mer, R.Check.Date, R.checker.version)


# create output list with all existing test data frames, etc
output <- list(metadata = metadata,
               surveyor = surveyor,
               volume = volume,
               year = year,
               state = state,
               county = county,
               baseline = baseline,
               meridian = meridian,
               township = township,
               townshipdir = townshipdir,
               range = range,
               rangedir = rangedir,
               typecorner = typecorner,
               section.combo = if(nrow(section.combo)>0) section.combo else "none",
               section.comboA = if(nrow(section.comboA)>0) section.comboA else "none",
               section.comboB = if(nrow(section.comboB)>0) section.comboB else "none",
               qsection.comboA = if(nrow(qsection.comboA)>0) qsection.comboA else "none",
               qsection.comboB = if(nrow(qsection.comboB)>0) qsection.comboB else "none",
               section.dup = if(nrow(section.dup)>0) section.dup else "none",
               valid.interiorsection = if(nrow(valid.interiorsection)>0) valid.interiorsection else "none",
               diameter.check.1 = if(nrow(diameter.check)>0) diameter.check else "none",
               diameter.check.2 = if(nrow(diameter.check2)>0) diameter.check2 else "none",
               diameter.check.3 = if(nrow(diameter.check3)>0) diameter.check3 else "none",
               diameter.check.4 = if(nrow(diameter.check4)>0) diameter.check4 else "none",
               bearing.1 = if(nrow(match.view)>0) match.view else "none",
               bearing.2 = if(nrow(match.view2)>0) match.view2 else "none",
               bearing.3 = if(nrow(match.view3)>0) match.view3 else "none",
               bearing.4 = if(nrow(match.view4)>0) match.view4 else "none",
               degrees.1 = if(nrow(acute.view)>0) acute.view else "none",
               degrees.2 = if(nrow(acute.view2)>0) acute.view2 else "none",
               degrees.3 = if(nrow(acute.view3)>0) acute.view3 else "none",
               degrees.4 = if(nrow(acute.view4)>0) acute.view4 else "none",
               bearingdir1 = if(nrow(compass.view)>0) compass.view else "none",
               bearingdir2 = if(nrow(compass.view2)>0) compass.view2 else "none",
               bearingdir3 = if(nrow(compass.view3)>0) compass.view3 else "none",
               bearingdir4 = if(nrow(compass.view4)>0) compass.view4 else "none",
               decimal.links1 = if(nrow(decimalstree1)>0) decimalstree1 else "none",
               decimal.links2 = if(nrow(decimalstree2)>0) decimalstree2 else "none",
               decimal.links3 = if(nrow(decimalstree3)>0) decimalstree3 else "none",
               decimal.links4 = if(nrow(decimalstree4)>0) decimalstree4 else "none",
               refcornertest = if(nrow(refcornertest)>0) refcornertest else "none",
               section.refcorner = if(nrow(section.combo.ref)>0) section.combo.ref else "none",
               qr.section.refcorner = if(nrow(qsection.combo.ref)>0) qsection.combo.ref else "none",
               sectionCheck = if(nrow(sectionCheck)>0) sectionCheck else "none",
               missing.corner = if(nrow(missing.corner)>0) missing.corner else "none",
               missing.border = if(nrow(missing.border)>0) missing.border else "none",
               extra.NWborder = if(exists('NWbordercorners.test')) NWbordercorners.test else "none",
               cornerid.dups = if(nrow(cornerid.dups)>0) cornerid.dups else"none",
               recheck = if(nrow(recheck)>0) recheck else "none",
               verbatim.trees = "sort by verbatim trees and check for weird names (e.g., Di which should be Do or one post oaks with lots of pin oaks, etc.) - Done?",
               no.tree.entries = "remove 'YES' from notree column for entries with trees - Done?",
               illegible.missing = "search for entries with 88888s or 99999s. make sure there are notes in the General Notes column explaining these entries - Done?",
               no.data = "check that there is a description of why there is no data (e.g., page missing, indian territory, etc)",
               no.tree = "check corners marked no trees has include details about condition of corner (e.g., post in mound, post, etc)",
               water.wet = "check corners marked as water or wet that there is a description for why it has water or is wet (e.g., pond, swamp, marsh, etc)",
               reminder = "remove 'yes' in the 'notree' column for GLO v1.0 data"
)

#this removes all the output entries that have "none" or 0 entries. 
output_reduced <- output[sapply(output, function(x) class(x) != 'character' || x != 'none')]

# Print output 

sink(file = paste("./Township Data Checks/Output/", township.name, ".txt", sep=""))
output_reduced
sink() # stops sinking

##sort by cornerid then re-write the town file to make the 1/4 section correction and adding the NAs to the bearing and bearingdir entries.
town = town[order(town$cornerid),] 
write.csv(town, paste0("./Qualtrics Checks By Reader/Checked Townships/QA_QC Checks/", township.name, "_Checked_RINPROGRESS.csv"), row.names = FALSE)


###############################################################################################################
### Full List of Changes Made To the TownshipDataChecker

##### Version 1.6 updates #####
# Jody added no.data, no.trees and water.wet to things for the R checkers to look at during the R check.
# Jody Peters, August 7, 2015

###### Version 1.6 updates #####
#Jody removed uneccesary questions in the output - check to make sure township range match (we select by township range in MySQL so it will only pull matching entries),
#scan for rare species (this is covered in the verbatim tree check)
#Jody Peters, October 29, 2015

#### Version 1.5 updates #####
# Jody added large.diameters, large.degrees, illegible.missing and verbatim.trees as things to check at the end of the Output pdf file
# Jody Peters, July 16, 2015

###### Version 1.4 updates ######
# Angharad Hamlin and Jody Peters, October 28, 2014
# 1) Changed the check for looking for decimals in chainstree columns. In the previous 1.3 version the R checker looked for any decimals (code starts on line 431). 
# Now we are looking for any decimals that are not 0, 0.25, 0.5 and 0.75.  These decimal values are okay because they have become commonly entered by the surveyors.
# 2) Also changed TypeCorner Checks.  It had been giving errors, so have corrected the errors.  The Output no longer lists the typecorner.cornerid.mismatch error, 
# instead it is labeled as SectionCheck and will tell if the error is for the Section, Quarter Section, or Township, what the cornerID is and what the entryID is.
# 3) Added code to the Section.Combo and QSection.Combo that takes out all the rows which have NAs in the cornerid column (previously the output was super long 
# and included tons of NAs)


#  Jill Deines, December 5, 2012
#  All commented out "test" values apply to Massac County township 14S 4 E, which was used to develop this script.
#  These are preserved for future confirmation of working code if necessary.
# town <- read.table("Massac_14S_4E_Export.csv", header = TRUE, sep = ",", na.strings = "NULL")
# township.name = "Massac 14S 4E"


##### Verison 1.3 updates #####
# Angharad Hamlin and Jody Peters, May 9, 2014
# Added logic tests to check that corner IDs match section numbers for both sections and quarter sections. Uses dplyr library to 
# join tables of possible section numbers to the township data. This package will need to be downloaded in order for the code to
# work.