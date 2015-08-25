## Version 1.4 ##

#  This script is an automated data checker for Notre Dame PLS data.  It should be run on
#  completed townships by exporting that township from MySQL to a csv.  The data checker will
#  flag logical data entry errors for further investigation by humans.

#  For detailed instructions on how to use this script, see ____ on the Notre Dame
#  Settlement Vegetation Google Site here: _________

#  All commented out "test" values apply to Massac County township 14S 4 E, which was used to develop this script.
#  These are preserved for future confirmation of working code if necessary.
# town <- read.table("Massac_14S_4E_Export.csv", header = TRUE, sep = ",", na.strings = "NULL")
# township.name = "Massac 14S 4E"

#  Jill Deines, December 5, 2012

#  updates needed:
# don't flag bearing entries that are entered using "cardinal direction" protocol
# a more in depth check to make sure section #'s, direction, and chains match cornerid number
# I'm sure there are others...

# Verison 1.3 updates

# Added logic tests to check that corner IDs match section numbers for both sections and quarter sections. Uses dplyr library to 
# join tables of possible section numbers to the township data. This package will need to be downloaded in order for the code to
# work.

# Angharad Hamlin and Jody Peters, May 9, 2014

# Verison 1.4 updates

# 1) Changed the check for looking for decimals in chainstree columns. In the previous 1.3 version the R checker looked for any decimals (code starts on line 431). 
# Now we are looking for any decimals that are not 0, 0.25, 0.5 and 0.75.  These decimal values are okay because they have become commonly entered by the surveyors.
# 2) Also changed TypeCorner Checks.  It had been giving errors, so have corrected the errors.  The Output no longer lists the typecorner.cornerid.mismatch error, 
# instead it is labeled as SectionCheck and will tell if the error is for the Section, Quarter Section, or Township, what the cornerID is and what the entryID is.
# 3) Added code to the Section.Combo and QSection.Combo that takes out all the rows which have NAs in the cornerid column (previously the output was super long 
# and included tons of NAs)

# Angharad Hamlin and Jody Peters, October 28, 2014

# Version 1.5 updates

# Jody added large.diameters, large.degrees, illegible.missing and verbatim.trees as things to check at the end of the Output pdf file

# Jody Peters, July 16, 2015

# Version 1.6 updates

# Jody added no.data, no.trees and water.wet to things for the R checkers to look at during the R check.

# Jody Peters, August 7, 2015

##  SET WORKING DIRECTORY to tell R where to look for the csv files.  Only run the line below
#  that applies to the computer you are working on (or add your own).  The file path should
#  lead to the "Township Data Checks" folder on your computer.
#  setwd("C:/Dropbox/PalEON Lab Team/Township Data Checks")  #lab GIS computer
#  setwd("C:/Users/jmurray7/Dropbox/PalEON Lab Team/Township Data Checks")  # Jody's work computer
#  setwd("/Users/paleolab/Dropbox/PalEON Lab Team/Township Data Checks")  #lab MAC
#  setwd("C:/Users/paleolab/Dropbox/PalEON Lab Team/Township Data Checks") #middle comp and laptop




## START code here for each township ##







# REMOVE ANY VARIABLES STORED IN THE ENVIRONMENT (for running code on multiple townships during one session)
rm(list=ls(all=TRUE)) 






# insert version number into report
R.checker.version <- "Version 1.5"





##  READ IN EXPORTED TOWNSHIP .csv file (see google site for instructions on creating
#  and saving this file).  I used read.table to deal with MySQL using "NULL" 

#  ## WILL NEED TO CHANGE FILE NAME FOR YOUR TOWNSHIP ##
town <- read.table("Lagrange_36N_10E_Export.csv", header = TRUE, sep=",", na.strings = "NULL")
township.name <- "Lagrange 36N 10E"









## HEY WAIT!  DID YOU READ IN THE RIGHT TOWNSHIP FILE AND NAME ABOVE? ##













## TYPOS OR MISSING VALUES ##

# list levels of identification variables.  These lists can then be evaluated for typos (will create
# 2 levels and be easy to spot), blank values (""), etc, to identify items to go back into 
# MySQL and adjust if needed
surveyor  <- levels(town$surveyor_name)
volume <- levels(as.factor(town$volume))
year <- levels(as.factor(town$year))
state <- levels(town$state)
baseline <- levels(as.factor(town$baseline))
meridian <- levels(as.factor(town$meridian))
townshipdir <- levels(town$townshipdir)
rangedir <- levels(town$rangedir)
township <- levels(as.factor(town$township))
range <- levels(as.factor(town$rangenum))








## LOGIC CHECKS ##



## CHECK valid sectiona and sectionb combinations (including missing values)

# read in table of valid section combinations.  For each section 1-36 (column 1), this table
# provides the valid section corner matches
combo <- read.csv("sectionCombos.csv")

# town$sectiona[2] = 8    # test data
# town$sectionb[1] = 16   # test data
# town$sectiona[3] = NA   # test data
# town$sectionb[4] = ""   # test data

# create a vector for the subsequent 'for loop' to put data into
is.combo <- rep(NA,length(town$sectionb))

# run loop to test if sectionb is a valid compliment to sectiona for each row
for(i in 1:length(town$sectionb)) {
  is.combo[i]<-town$sectionb[i] %in% combo[town$sectiona[i],2:5]}

# filter out NA's in is.combo (which represent interiorsections) and make report of failed combinations
ifelse(is.combo == FALSE & is.na(town$interiorsection),
       section.combo <- data.frame(entry_id = town[is.combo == FALSE & is.na(town$interiorsection),]$entry_id,
                                   error = "  sectiona and sectionb combination invalid",
                                   sectiona = town[is.combo == FALSE & is.na(town$interiorsection),]$sectiona,
                                   sectionb = town[is.combo == FALSE & is.na(town$interiorsection),]$sectionb),
       "ok"
)

## CHECK valid section and section corner ID
# This check will make sure that the number entered for Section A (B) is actually a section that could be found for the entered corner ID.

# SECTION A

# read in table of valid section and section corner ID combinations.  For each corner ID the table provides valid section number combinations.
combo1 <- read.csv("SectionCornerIDs.csv")

# create a vector for the subsequent 'for loop' to put data into
is.combo1 <- rep(NA,length(town$sectiona))

# load dplr so that we can use left.join
library(dplyr)

# create new table that includes both combo1 information and town information. matched by cornerid.
matched1= left_join(combo1,town,by='cornerid')

# check to see if section a is found in columns 2-5 (possible sections for each cornerid)
for(i in 1:length(town$sectiona)) {
  
  is.combo1[i]<-matched1$sectiona[i] %in% matched1[i,2:5]}

# filter out NA's in is.combo (which represent interiorsections/borders) and make report of failed combinations
ifelse(is.combo1 == FALSE & is.na(matched1$interiorsection),
       section.comboA <- data.frame(entry_id = matched1[is.combo1 == FALSE & is.na(matched1$interiorsection),]$entry_id,
                                    error = "  sectiona and cornerID invalid",
                                    sectiona = matched1[is.combo1 == FALSE & is.na(matched1$interiorsection),]$sectiona,
                                    cornerid = matched1[is.combo1 == FALSE & is.na(matched1$interiorsection),]$cornerid),
       "ok"
)

####this takes out all the rows which have NAs in the cornerid column, which is column 4 (and is coded by [,4]), this makes it so the list isn't super 
## long for the Output file
section.comboA = section.comboA[!is.na(section.comboA[,4]),]


# SECTION B

# read in table of valid section and section corner ID combinations.  For each corner ID the table provides valid section number combinations.
combo2 <- read.csv("SectionCornerIDs.csv")

# create a vector for the subsequent 'for loop' to put data into
is.combo2 <- rep(NA,length(town$sectionb))

# create new table that includes both combo1 information and town information. matched by cornerid.
matched2= left_join(combo2,town,by='cornerid')

# check to see if section b is found in columns 2-5 (possible sections for each cornerid)
for(i in 1:length(town$sectionb)) {
  
  is.combo2[i]<-matched2$sectionb[i] %in% matched2[i,2:5]}

# filter out NA's in is.combo (which represent interiorsections/borders) and make report of failed combinations
ifelse(is.combo2 == FALSE & is.na(matched2$interiorsection),
       section.comboB <- data.frame(entry_id = matched2[is.combo2 == FALSE & is.na(matched2$interiorsection),]$entry_id,
                                    error = "  sectionb and cornerID invalid",
                                    sectionb = matched2[is.combo2 == FALSE & is.na(matched2$interiorsection),]$sectionb,
                                    cornerid = matched2[is.combo2 == FALSE & is.na(matched2$interiorsection),]$cornerid),
       "ok"
)

####this takes out all the rows which have NAs in the cornerid column, which is column 4 (and is coded by [,4]), this makes it so the list isn't super 
## long for the Output file
section.comboB = section.comboB[!is.na(section.comboB[,4]),]

# CHECK valid section and quarter section corner ID
# This test is the same as above but will make sure the quarter section ID corresponds to the section numbers entered.  

# 1/4 SECTION A

# read in table of valid sections and quartersection ID combinations.  For each quartersection corner ID the table provides valid section number combinations.
combo3 <- read.csv("QuarterSectionCornerIDs.csv")

# create a vector for the subsequent 'for loop' to put data into
is.combo3 <- rep(NA,length(town$sectiona))

# match quarter section corner ids and township file
matched3= left_join(combo3,town,by='cornerid')

# check to see if section a is found in columns 2-3 (possible sections for each cornerid)
for(i in 1:length(matched3$sectiona)) {
  
  is.combo3[i]<-matched3$sectiona[i] %in% matched3[i,2:3]}

# filter out NA's in is.combo (which represent interiorsections) and make report of failed combinations
ifelse(is.combo3 == FALSE & is.na(matched3$interiorsection),
       qsection.comboA <- data.frame(entry_id = matched3[is.combo3 == FALSE & is.na(matched3$interiorsection),]$entry_id,
                                     error = "  sectiona and cornerID invalid",
                                     sectiona = matched3[is.combo3 == FALSE & is.na(matched3$interiorsection),]$sectiona,
                                     cornerid = matched3[is.combo3 == FALSE & is.na(matched3$interiorsection),]$cornerid),
       "ok"
)

####this takes out all the rows which have NAs in the cornerid column, which is column 4 (and is coded by [,4]), this makes it so the list isn't super 
## long for the Output file
qsection.comboA = qsection.comboA[!is.na(qsection.comboA[,4]),]


# 1/4 SECTION B

# read in table of valid sections and quartersection ID combinations.  For each quartersection corner ID the table provides valid section number combinations.
combo4 <- read.csv("QuarterSectionCornerIDs.csv")

# create a vector for the subsequent 'for loop' to put data into
is.combo4 <- rep(NA,length(town$sectionb))

# match quarter section corner ids and township file
matched4= left_join(combo4,town,by='cornerid')

# check to see if section b is found in columns 2-3 (possible sections for each cornerid)
for(i in 1:length(matched4$sectionb)) {
  
  is.combo4[i]<-matched4$sectionb[i] %in% matched4[i,2:3]}

# filter out NA's in is.combo (which represent interiorsections) and make report of failed combinations
ifelse(is.combo4 == FALSE & is.na(matched4$interiorsection),
       qsection.comboB <- data.frame(entry_id = matched4[is.combo4 == FALSE & is.na(matched4$interiorsection),]$entry_id,
                                     error = "  sectionb and cornerID invalid",
                                     sectionb = matched4[is.combo4 == FALSE & is.na(matched4$interiorsection),]$sectionb,
                                     cornerid = matched4[is.combo4 == FALSE & is.na(matched4$interiorsection),]$cornerid),
       "ok"
)

#### this takes out all the rows which have NAs in the cornerid column, which is column 4 (and is coded by [,4]), this makes it so the list isn't super 
## long for the Output file
qsection.comboB = qsection.comboB[!is.na(qsection.comboB[,4]),]



## CHECK that there aren't entries for both sectiona, section b, and interior section.  
# Not sure if this is possible in the GLO form, but Kate had it on the data check list and I suppose
# anyone could enter this in the SQL database

# town[97,16] = 23 # test data
# town[98,17] = 43 # test data
# town[97,17] = 34 # test data
# town[1,18] = 34 # test data

ifelse(!(is.na(town$interiorsection)) & ( !(is.na(town$sectiona)) | !(is.na(town$sectionb))),
       section.dup <- data.frame(entry_id = town[!(is.na(town$interiorsection)) & ( !(is.na(town$sectiona)) | !(is.na(town$sectionb))),]$entry_id,
                                 error = "  values in sectiona, sectionb, and interiorsection not allowed"),
       "ok"
)




## CHECK that all interiorsection section entries are valid section numbers touching border

# create vector of valid interior sections
int.section <- as.integer(c(1,2,3,4,5,6,7,18,19,30,31,32,33,34,35,36,25,24,13,12))

# town$interiorsection[1] = 9 # test data

# check if interiorsection data entry is within possible values
test <- town$interiorsection %in% int.section

# filter out NA's in test (which represent non-border/interiorsections) and make report of invalid interiorsections
ifelse(test == FALSE & !is.na(town$interiorsection),
       valid.interior <- data.frame(entry_id = town[test == FALSE & !is.na(town$interiorsection),]$entry_id,
                                    error = "  invalid interior section",
                                    interiorsection = town[test == FALSE & !is.na(town$interiorsection),]$interiorsection),
       "ok"
)




## CHECK bearing and degrees - 
# every bearing has a degrees value (and vice versa), bearing entries are N/S only, degrees are < 90


## Tree 1

# levels(town$bearing) <- c(levels(town$bearing), "W")   # test data
# town$bearing[1] = "W"     # test data
# town$degrees[2] = 100	    # test data
# town$degrees[4] = 99999   # test data	
# town$bearing[5] = ""	    # test data	
# town$degrees[6] = 88888   # test data

# test to see if the vector of true/falses match for:
# 1 - If degrees has value (True = value, False - NA/no data)
# 2 - if bearing has value (True = 1 of 4 possible values (N, S, 99999, 88888); False = no data/blank or invalid value)
match <- !is.na(town$degrees) == (town$bearing == "N" | town$bearing == "S" | town$bearing == "99999" | town$bearing == "88888")


# test to see if value of degrees is less than 90 (88888 and 99999 also valid)
acute <- town$degrees <= 90 | town$degrees == 99999 | town$degrees == 88888


# combine match and acute tests to generate error report
ifelse(match & acute | is.na(match & acute),
       "ok",
       bearing <- data.frame(entry_id = town[!(match & acute | is.na(match & acute)),]$entry_id, 
                             error = "   tree 1 missing/invalid value in bearing or degrees",
                             bearing = town[!(match & acute | is.na(match & acute)),]$bearing, 
                             degrees = town[!(match & acute | is.na(match & acute)),]$degrees)
)


## repeat for tree 2
match2 <- !is.na(town$degrees2) == (town$bearing2 == "N" | town$bearing2 == "S" | town$bearing2 == "99999" | town$bearing2 == "88888")
acute2 <- town$degrees2 <= 90 | town$degrees2 == 99999 | town$degrees2 == 88888
ifelse(match2 & acute2 | is.na(match2 & acute2),
       "ok",
       bearing2 <- data.frame(entry_id = town[!(match2 & acute2 | is.na(match2 & acute2)),]$entry_id, 
                              error = "   tree 2 missing/invalid value in bearing2 or degrees2",
                              bearing2 = town[!(match2 & acute2 | is.na(match2 & acute2)),]$bearing2, 
                              degrees2 = town[!(match2 & acute2 | is.na(match2 & acute2)),]$degrees2)
)


## repeat for tree 3
match3 <- !is.na(town$degrees3) == (town$bearing3 == "N" | town$bearing3 == "S" | town$bearing3 == "99999" | town$bearing3 == "88888")
acute3 <- town$degrees3 <= 90 | town$degrees3 == 99999 | town$degrees3 == 88888
ifelse(match3 & acute3 | is.na(match3 & acute3),
       "ok",
       bearing3 <- data.frame(entry_id = town[!(match3 & acute3 | is.na(match3 & acute3)),]$entry_id, 
                              error = "   tree 3 missing/invalid value in bearing3 or degrees3",
                              bearing3 = town[!(match3 & acute3 | is.na(match3 & acute3)),]$bearing3, 
                              degrees3 = town[!(match3 & acute3 | is.na(match3 & acute3)),]$degrees3)
)


## repeat for tree 4
match4 <- !is.na(town$degrees4) == (town$bearing4 == "N" | town$bearing4 == "S" | town$bearing4 == "99999" | town$bearing4 == "88888")
acute4 <- town$degrees4 <= 90 | town$degrees4 == 99999 | town$degrees4 == 88888
ifelse(match4 & acute4 | is.na(match4 & acute4),
       "ok",
       bearing4 <- data.frame(entry_id = town[!(match4 & acute4 | is.na(match4 & acute4)),]$entry_id, 
                              error = "   tree 4 missing/invalid value in bearing4 or degrees4",
                              bearing4 = town[!(match4 & acute4 | is.na(match4 & acute4)),]$bearing4, 
                              degrees4 = town[!(match4 & acute4 | is.na(match4 & acute4)),]$degrees4)
)




## CHECK bearingdir and chainstree
# every bearingdir corresponds to a chainstree value (and vice versa), and bearingdir
# are always E and W

# town$chainstree[3] = 99999  # test data 

# test to see if the vector of true/falses match for:
# 1 - If chainstree has value (True = value, False - NA/no data)
# 2 - if bearingdir has value (True = 1 of 4 possible values (N, S, 99999, 88888); False = no data/blank or invalid value)
compass <- !is.na(town$chainstree) == (town$bearingdir == "E" | town$bearingdir == "W" | town$bearingdir == "99999" | town$bearingdir == "88888")

# generate report por favor
ifelse(compass | is.na(compass),
       "ok",
       tree.location <- data.frame(entry_id = town[(compass == FALSE & !is.na(compass)),]$entry_id,
                                   error = "  tree 1 missing/invalid bearingdir or chainstree",
                                   bearingdir = town[(compass == FALSE & !is.na(compass)),]$bearingdir,
                                   chainstree = town[(compass == FALSE & !is.na(compass)),]$chainstree)
)


## repeat for tree 2
compass2 <- !is.na(town$chainstree2) == (town$bearingdir2 == "E" | town$bearingdir2 == "W" | town$bearingdir2 == "99999" | town$bearingdir2 == "88888")
ifelse(compass2 | is.na(compass2),
       "ok",
       tree.location.2 <- data.frame(entry_id = town[(compass2 == FALSE & !is.na(compass2)),]$entry_id,
                                     error = "  tree 2 missing/invalid bearingdir2 or chainstree2",
                                     bearingdir2 = town[(compass2 == FALSE & !is.na(compass2)),]$bearingdir2,
                                     chainstree2 = town[(compass2 == FALSE & !is.na(compass2)),]$chainstree2)
)


## repeat for tree 3
compass3 <- !is.na(town$chainstree3) == (town$bearingdir3 == "E" | town$bearingdir3 == "W" | town$bearingdir3 == "99999" | town$bearingdir3 == "88888")
ifelse(compass3 | is.na(compass3),
       "ok",
       tree.location.3 <- data.frame(entry_id = town[(compass3 == FALSE & !is.na(compass3)),]$entry_id,
                                     error = "  tree 3 missing/invalid bearingdir3 or chainstree3",
                                     bearingdir3 = town[(compass3 == FALSE & !is.na(compass3)),]$bearingdir3,
                                     chainstree3 = town[(compass3 == FALSE & !is.na(compass3)),]$chainstree3)
)


## repeat for tree 4
compass4 <- !is.na(town$chainstree4) == (town$bearingdir4 == "E" | town$bearingdir4 == "W" | town$bearingdir4 == "99999" | town$bearingdir4 == "88888")
ifelse(compass4 | is.na(compass4),
       "ok",
       tree.location.4 <- data.frame(entry_id = town[(compass4 == FALSE & !is.na(compass4)),]$entry_id,
                                     error = "  tree 4 missing/invalid bearingdir4 or chainstree4",
                                     bearingdir4 = town[(compass4 == FALSE & !is.na(compass4)),]$bearingdir4,
                                     chainstree4 = town[(compass4 == FALSE & !is.na(compass4)),]$chainstree4)
)











## CHECK for non-translated decimals in distance measurements (ie, 2.1 "links" should read 210 links, as it actually
# stands for 2.1 chains, and there are 100 links in each chain...metric-like surveyors, what what!)

# check for decimal values other than 0.0, 0.25, 0.50 and 0.75 and return entry id
chain1 <-data.frame(floor(town$chainstree))
chaindiff <-(town$chainstree-chain1)
newchain <-cbind(town, chaindiff)

decimalstree1A <-newchain[newchain$floor.town.chainstree. != 0.00 & newchain$floor.town.chainstree. !=0.25 & newchain$floor.town.chainstree. !=0.50 & newchain$floor.town.chainstree. !=0.75,]
decimalstree1B=cbind(decimalstree1A$entry_id,round(decimalstree1A$floor.town.chainstree., digits=2),"check chainstree decimal")
colnames(decimalstree1B)=c("entry_id","chaindiff","error")

# tree 2
chain2 <-data.frame(floor(town$chainstree2))
chaindiff2 <-(town$chainstree2-chain2)
newchain2 <-cbind(town, chaindiff2)

decimalstree2A <-newchain2[newchain2$floor.town.chainstree2. != 0.50 & newchain2$floor.town.chainstree2. !=0.25 & newchain2$floor.town.chainstree2. !=0.00 & newchain2$floor.town.chainstree2. !=0.75,]
decimalstree2B=cbind(decimalstree2A$entry_id,round(decimalstree2A$floor.town.chainstree2., digits=2),"check chainstree decimal")
colnames(decimalstree2B)=c("entry_id","chaindiff","error")

# tree 3
chain3 <-data.frame(floor(town$chainstree3))
chaindiff3 <-(town$chainstree3-chain3)
newchain3 <-cbind(town, chaindiff3)

decimalstree3A <-newchain3[newchain3$floor.town.chainstree3. != 0.50 & newchain3$floor.town.chainstree3. !=0.25 & newchain3$floor.town.chainstree3. !=0.00 & newchain3$floor.town.chainstree3. !=0.75,]
decimalstree3B=cbind(decimalstree3A$entry_id,round(decimalstree3A$floor.town.chainstree3., digits=3),"check chainstree decimal")
colnames(decimalstree3B)=c("entry_id","chaindiff","error")

# tree 4
chain4 <-data.frame(floor(town$chainstree4))
chaindiff4 <-(town$chainstree4-chain4)
newchain4 <-cbind(town, chaindiff4)

decimalstree4A <-newchain4[newchain4$floor.town.chainstree4. != 0.50 & newchain4$floor.town.chainstree4. !=0.25 & newchain4$floor.town.chainstree4. !=0.00 & newchain4$floor.town.chainstree4. !=0.75,]
decimalstree4B=cbind(decimalstree4A$entry_id,round(decimalstree4A$floor.town.chainstree4., digits=3),"check chainstree decimal")
colnames(decimalstree4B)=c("entry_id","chaindiff","error")


# combine date frames into one output
decimal.links <- rbind(
  if(exists('decimalstree1B')) decimalstree1B else none,
  if(exists('decimalstree2B')) decimalstree2B else none,
  if(exists('decimalstree3B')) decimalstree3B else none,
  if(exists('decimalstree4B')) decimalstree4B else none)

#this takes out all the rows with NA values, so the list isn't super long for the Output file
decimal.links = decimal.links[!is.na(decimal.links[,1]),]







## REFERENCE CORNER CHECKS ##
# Check for invalid values in "refcorner" or NAs

# assign false values for testing
# town[1,20] = 100000
# town[2,20] = 700800
# put dummy values in for NA's for testing
# town[43,20] = 700600
# town[44,20] = 500600
# town[45,20] = 400100
# town[64,20] = 300300

# create vector of valid reference corner ids
reference.corners <- c(100100, 100200, 100300, 100400, 100500, 100600, 100700, 200100, 200200,
                       200300, 200400, 200500, 200600, 200700, 300100, 300200, 300300, 300400, 300500, 300600,
                       300700, 400100, 400200, 400300, 400400, 400500, 400600, 400700, 500100, 500200, 500300,
                       500400, 500500, 500600, 500700, 600100, 600200, 600300, 600400, 600500, 600600, 600700,
                       700100, 700200, 700300, 700400, 700500, 700600, 700700)

# Check for invalid values in "refcorner" or NAs
# For invalid values/NAs, create a data frame "refcorner.bounds" with the entry id, error, and 
# refcorner value in question
ifelse(!(town$refcorner %in% reference.corners),
       refcorner.bounds <- data.frame(entry_id = town[!(town$refcorner %in% reference.corners),]$entry_id, 
                                      error="invalid refcorner or NA", refcorner = town[!(town$refcorner %in% reference.corners),]$refcorner),
       "ok")



# Check for duplicate entries via refcorner, chains, traveldir, and typecorner 
ifelse(duplicated(town[,c("refcorner", "chains", "traveldir", "typecorner")]),
       row.duplicate <- data.frame(entry_id = town[(duplicated(town[,c("refcorner","chains","traveldir", "typecorner")])),]$entry_id, 
                                   error="  possible duplicate entry",
                                   refcorner=town[(duplicated(town[,c("refcorner","chains","traveldir", "typecorner")])),]$refcorner),
       "ok")








## DATA CORNER CHECKS ##

# This section checks that data was collected for each section corner and 1/4 section corner in the township.
# To improve: first run logic checks on appropriate corner IDs, then run this?

# add levels for Temporary and Intersection to typecorner factor in the event the township has 
# none of these entered
#levels(town$typecorner) <- c(levels(town$typecorner), "Intersection", "Temporary")







## TYPECORNER checks
# Will West helped JP recode this section.  Previously it had been giving errors for the section and township output.

# Check to make sure that every row with (1/4) Section, Section, or Township in "typecorner" has a valid cornerid
# note: when "typecorner" has a "null" value in SQL, this doesn't work properly and reports mismatches in entry-ID's = NA.
# not sure why typecorner was null in these instances... (Stephenson 27N8E)?


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
new_sectionmerge2=cbind(new_sectionmerge$entry_id,new_sectionmerge$cornerid,"mismatched cornerid and section", "Section")
new_sectionmergeNA=cbind("NA", "NA", "mismatched cornerid and section", "Section")
if(dim(new_sectionmerge2)[2]<4){new_sectionmerge2=new_sectionmergeNA} else{new_sectionmerge2=new_sectionmerge2}
colnames(new_sectionmerge2)=c("entry_id","cornerid","error","Corner Type")


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
new_quartermerge2=cbind(new_quartermerge$entry_id,new_quartermerge$cornerid,"mismatched cornerid and section", "Quarter Section")
new_quartermergeNA=cbind("NA", "NA", "mismatched cornerid and section", "Quarter Section")
if(dim(new_quartermerge2)[2]<4){new_quartermerge2=new_quartermergeNA} else{new_quartermerge2=new_quartermerge2}
colnames(new_quartermerge2)=c("entry_id","cornerid","error","Corner Type")


##Townships Check

township.corners <- c(100100,100700,700100,700700)

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
new_townshipmerge2=cbind(new_townshipmerge$entry_id,new_townshipmerge$cornerid,"mismatched cornerid and section","Township")
new_townshipmergeNA=cbind("NA", "NA", "mismatched cornerid and section", "Township")
if(dim(new_townshipmerge2)[2]<4){new_townshipmerge2=new_townshipmergeNA} else{new_townshipmerge2=new_townshipmerge2}
colnames(new_townshipmerge2)=c("entry_id","cornerid","error", "Corner Type")


sectionCheck=rbind(new_sectionmerge2,new_quartermerge2, new_townshipmerge2)







## Corner completeness check ##

# add test values
# town [1,24] = "Intersection" # add test factors
# town [2,24] = "Temporary"		# add test factors

# remove rows with Intersection or Temporary since these have no data and therefore do not 
# count as corner data
town1 <- town[!(town$typecorner == "Intersection" | town$typecorner == "Temporary"),]


# Internal Corners #

# create a vector with internal corner id values (non borders)
internal.corners <- c(140200, 140300, 140400, 140500, 140600, 200140, 200200, 240200, 200240, 200300,
                      240300, 200340, 200400, 240400, 200440, 200500, 240500, 200540, 200600, 240600, 200640, 300140, 
                      300200, 340200, 300240, 300300, 340300, 300340, 300400, 340400, 300440, 300500, 340500, 300540, 
                      300600, 340600, 300640, 400140, 400200, 440200, 400240, 400300, 440300, 400340, 400400, 440400, 
                      400440, 400500, 440500, 400540, 400600, 440600, 400640, 500140, 500200, 540200, 500240, 500300, 
                      540300, 500340, 500400, 540400, 500440, 500500, 540500, 500540, 500600, 540600, 500640, 600140, 
                      600200, 640200, 600240, 600300, 640300, 600340, 600400, 640400, 600440, 600500, 640500, 600540, 
                      600600, 640600, 600640) 

# test to see if all internal corners are in town1 dataset; if not, create dataframe with
# each missing corner id and error message
ifelse(internal.corners %in% town1$cornerid,
       "ok",
       missing.corner <- data.frame(cornerid = internal.corners[!(internal.corners %in% town1$cornerid)],
                                    error = "cornerid not found in township data when temporary and intersections excluded")
)


# South and East borders #

# create a vector with south and east border corner id values
# ** is 700100 included in township, or is it 100100 of neighbor (and vice versa)?  and other such township
# corner questions (but mostly just this one)
SEborder.corners <- c(100100, 140100, 200100, 240100, 300100, 340100, 400100, 440100, 500100, 540100, 600100,
                      640100, 700100, 700140, 700200, 700240, 700300, 700340, 700400, 700440, 700500, 700540, 700600, 700640)

# test to see if all SE border corners are in town1 dataset; if not, create dataframe with
# each missing corner id and error message
ifelse(SEborder.corners %in% town1$cornerid,
       "ok",
       missing.border <- data.frame(cornerid = SEborder.corners[!(SEborder.corners %in% town1$cornerid)],
                                    error = "cornerid not found in township data when temporary and intersections excluded")
)


# North and West borders #
# ** not sure what to do with these once identified

# create a vector with north and west border corner id values
# **include 100100 here? 700700 is included here
NWborder.corners <- c(100100, 100140, 100200, 100240, 100300, 100340, 100400, 100440, 100500, 100540, 100600, 100640,
                      100700, 140700, 200700, 240700, 300700, 340700, 400700, 440700, 500700, 540700, 600700, 640700, 700700)

# list any NW border corners in town1 dataset and create dataframe listing cornerids
ifelse(NWborder.corners %in% town1$cornerid,
       extra.border <- data.frame(cornerid = NWborder.corners[NWborder.corners %in% town1$cornerid],
                                  message = "  north or west border point"),
       "ok")



# Duplicates # 

# remove NA's in cornerid (ie, non corner points)
# second logical test is for when R reads in town1$cornerid as factor, so blank is not NA
town6 <- town1[!is.na(town1$cornerid) & !town1$typecorner == "",]

ifelse(duplicated(town6$cornerid),
       corner.duplicate <- data.frame(entry_id = town6[(duplicated(town6$cornerid)),]$entry_id, 
                                      error="   possible duplicate corner (excludes temp and intersections)",
                                      cornerid = town6[(duplicated(town6$cornerid)),]$cornerid),
       "ok")






## CREATE LIST OF RECHECKS##

# create list of all entries flagged for recheck with reason and any fixed messages

# town[1,73] = "JD fixed" # test point for massac



# if flagged for recheck, list error and fixed message, if present
ifelse(town$recheck == "Y",
       recheck <- data.frame(entry_id = town[town$recheck == "Y",]$entry_id, 
                             reason = substring(as.character(town[town$recheck == "Y",]$reason),1, last = 45),
                             fixed = substring(as.character(town[town$recheck == "Y",]$fixed),1, last = 35)),
       "ok"
)










## CREATE LIST OF GENERAL NOTES? ##





## LIST ERRORS FOR OUTPUT ##

# create dummy dataframe for when test conditions above not met (ie, no error found)
# none <- data.frame(entry_id="none", error = "   no error found")  # commented out since this is now introducted 
# in typecorner checks


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
               baseline = baseline,
               meridian = meridian,
               township = township,
               townshipdir = townshipdir,
               range = range,
               rangedir = rangedir,
               duplicated.row = if(exists('row.duplicate')) row.duplicate else"none",         
               duplicated.corner = if(exists('corner.duplicate')) corner.duplicate else"none",
               section.dup = if(exists('section.dup')) section.dup else "none",
               section.combo = if(exists('section.combo')) section.combo else "none",
               invalid.refcorner = if(exists('refcorner.bounds')) refcorner.bounds else "none", 
               valid.interior = if(exists('valid.interior')) valid.interior else "none",
               no.corner.int = if(exists('missing.corner')) missing.corner else "none",
               missing.border = if(exists('missing.border')) missing.border else "none",
               extra.border = if(exists('extra.border')) extra.border else "none",
               SectionCheck = sectionCheck,
               section.comboA = if(exists('section.comboA')) section.comboA else "none",
               section.comboB = if(exists('section.comboB')) section.comboB else "none",
               qsection.comboA = if(exists('qsection.comboA')) qsection.comboA else "none",
               qsection.comboB = if(exists('qsection.comboB')) qsection.comboB else "none",
               bearing.degrees.1 = if(exists('bearing')) bearing else "none",
               bearing.degrees.2 = if(exists('bearing2')) bearing2 else "none",
               bearing.degrees.3 = if(exists('bearing3')) bearing3 else "none",
               bearing.degrees.4 = if(exists('bearing4')) bearing4 else "none",
               tree.location.1 = if(exists('tree.location')) tree.location else "none",
               tree.location.2 = if(exists('tree.location.2')) tree.location.2 else "none",
               tree.location.3 = if(exists('tree.location.3')) tree.location.3 else "none",
               tree.location.4 = if(exists('tree.location.4')) tree.location.4 else "none",
               decimals.in.links = decimal.links,
               recheck = if(exists('recheck')) recheck else "none",
               rare.species = "scan for rare species and make sure they match verbatim (vs dropdown list error) - Done?",
               township.range = "make sure all township/range numbers have two digits (ie, 02) - Done? ",
               large.diameters = "check any diameters over 60 inches. add note to fixed column that you checked it - Done?",
               large.degrees = "check that there are no degrees over 90, add note to fixed column if they are actually over 90 - Done?",
               illegible.missing = "search for entries with 88888s or 99999s. make sure there are notes in fixed column explaining these entries - Done?",
               verbatim.trees = "sort by verbatim trees and check for weird names (e.g., Di which should be Do or one post oaks with lots of pin oaks, etc.) - Done?",
               no.data = "check that there is a description of why there is no data (e.g., page missing, indian territory, etc)",
               no.tree = "check corners marked no tress has include details about condition of corner (e.g., post in mound, post, etc)",
               water.wet = "check corners marked as water or wet that there is a description for why it has water or is wet (e.g., pond, swamp, marsh, etc)"
)



# Print output 

sink(file = paste("Output/", township.name, ".txt", sep=""))
output
sink() # stops sinking