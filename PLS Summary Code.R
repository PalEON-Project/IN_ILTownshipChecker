###############################################################################
######## PLS Summary - USE RESULTS FOR QA/QC and for IN/IL PLS READMEs ########
###############################################################################

#created by Jody Peters to do a final QA/QC of the georferenced PLS data before
#posting it on the wiki/GitHUb and to get a summary of new townships entered in the Notre Dame database
#also use this to help update the ndil[in]pls_vXX README files


#QA/QC needs:
#1. Make sure all No tree, No data, Water, Wet have NA values in their diameter, degrees, chainstree columns for all 4 trees
#2. Check for duplicate tree entries
#3. check that L1_treex + species match up
#4. check that L3_treex + L1_treex match up and use this to update the paleonconversion csv on GitHub
#5. check for buckhorns (or any other weird tree names) when checking L1trees. If any go back and check those.
#6. check for duplicate x/y points and entryids
#7. check that there are entries for all state, surveyor, year, county 
#8. check that Water/Wet entries have no trees entered


#extra code in case you want to find a row with specific value in a column
IL[which(IL$entry_id == 679872),]

setwd("C:/Users/jmurray7/Dropbox/GIS PalEON/IL_IN_WI Unprojected")
rm(list=ls())

IL = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/ndilpls_v1.8-1.csv", header = TRUE, stringsAsFactors = FALSE)
IN = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7.csv", header = TRUE, stringsAsFactors = FALSE)

#57311 records in Illinois version 1.8-1


#remove Water, Wet, No data, and No tree entries so only entries with trees are included in the new dataframe
newIL <- IL[!(IL$L3_tree1 %in% c("Water","Wet","No data", "No tree")),]
#just for fun select only the Water, Wet, No data and No tree entries and make sure that the total for
#nontreeIL and newIL equals the total entries for IL
nontreeIL <- IL[(IL$L3_tree1 %in% c("Water","Wet","No data", "No tree")),]
#nontreeIL = 29003, newIL = 28308, IL = 57311. They match!


#sort newIL by trees to make sure all entries have trees in tree1 column
#sort ascending
newILsort = newIL[order(newIL$verbatim),] 
View(newILsort) #look at the output to see if there are trees in the first row

#sort descending
library(plyr)
newILsortdescending = arrange(newIL,desc(verbatim))
View(newILsortdescending) #look at the output to see if there are trees in the first row


#concatenate names,diameter and bearing info for tree1, tree2, tree3 and tree4 
uniquetree1 = paste0(newIL$verbatim,newIL$diameter,newIL$bearing,newIL$degrees,newIL$bearingdir,newIL$chainstree)
uniquetree2 = paste0(newIL$verbatim2,newIL$diameter2,newIL$bearing2,newIL$degrees2,newIL$bearingdir2,newIL$chainstree2)
uniquetree3 = paste0(newIL$verbatim3,newIL$diameter3,newIL$bearing3,newIL$degrees3,newIL$bearingdir3,newIL$chainstree3)
uniquetree4 = paste0(newIL$verbatim4,newIL$diameter4,newIL$bearing4,newIL$degrees4,newIL$bearingdir4,newIL$chainstree4)

newIL$uniquetree1 = uniquetree1
newIL$uniquetree2 = uniquetree2
newIL$uniquetree3 = uniquetree3
newIL$uniquetree4 = uniquetree4


temporary = newIL[,c("entry_id","uniquetree1","uniquetree2","uniquetree3","uniquetree4")]
#write.csv(temporary, file = "C:/Users/jmurray7/Desktop/ILTemp.csv", row.names = FALSE) #used this when I saw that there were uniquetrees with NANANANANANA instead of NANANA

######################################################################################################
## MAKE SURE ALL NO TREE, NO DATA, WATER AND WET ENTRIES HAVE NAs FOR THEIR DIAMETERS, DEGREES, AND ##
## CHAINSTREES FOR ALL 4 TREES #######################################################################
######################################################################################################

#Use the code below to check for 000 where there should be NA NA NA. If there are 000 values - correct in csv by changing
#the 0s to NAs
temp1 = temporary[order(temporary$uniquetree1),] 
View(temp1)
temp2 = temporary[order(temporary$uniquetree2),] 
View(temp2)
temp3 = temporary[order(temporary$uniquetree3),] 
View(temp3)
temp4 = temporary[order(temporary$uniquetree4),] 
View(temp4)


##########################################################################
## CHECK IF THERE ARE DUPLICATE ENTRIES OF TREE DATA AT THE SAME CORNER ##
##########################################################################

#remove entries with NANANA from tree 2 before you compare it to tree 1
temporary.2 <- temporary[!(temporary$uniquetree2 %in% c("NANANA")),]
#TREE1 vs TREE2
#compare TREE1 vs TREE2 - these will have the greatest chance of having duplicates
tree1_vs_tree2 = 1:length(temporary.2$uniquetree1)
tree1_vs_tree2 = temporary.2$uniquetree1==temporary.2$uniquetree2
temporary.2$tree1_vs_tree2 = tree1_vs_tree2

#this finds the duplicates in trees1 and trees2 for each entry
temporary.2[which(temporary.2$tree1_vs_tree2 == "TRUE"),]

#remove entries with NANANA from tree 3
temporary.3 <- temporary.2[!(temporary.2$uniquetree3 %in% c("NANANA")),]
#TREE3 vs TREE2
#look for duplicates for TREE3 compared to TREE2
tree2_vs_tree3 = 1:length(temporary.3$uniquetree2)
tree2_vs_tree3 = temporary.3$uniquetree2==temporary.3$uniquetree3
temporary.3$tree2_vs_tree3 = tree2_vs_tree3

#this finds the duplicates in TREE2 and TREE3 for each entry
temporary.3[which(temporary.3$tree2_vs_tree3 == "TRUE"),]

#TREE3 vs TREE1
#look for duplicates for TREE3 compared to TREE1
tree1_vs_tree3 = 1:length(temporary.3$uniquetree1)
tree1_vs_tree3 = temporary.3$uniquetree1==temporary.3$uniquetree3
temporary.3$tree1_vs_tree3 = tree1_vs_tree3

#this finds the duplicates in TREE1 and TREE3 for each entry
temporary.3[which(temporary.3$tree1_vs_tree3 == "TRUE"),]


#remove entries with NANANA from tree 4
temporary.4 <- temporary.3[!(temporary.3$uniquetree4 %in% c("NANANA")),]
#TREE4 vs TREE3
#look for duplicates for TREE4 compared to TREE3
tree4_vs_tree3 = 1:length(temporary.4$uniquetree4)
tree4_vs_tree3 = temporary.4$uniquetree4==temporary.4$uniquetree3
temporary.4$tree4_vs_tree3 = tree4_vs_tree3

#this finds the duplicates in TREE4 and TREE3 for each entry
temporary.4[which(temporary.4$tree4_vs_tree3 == "TRUE"),]

#TREE4 vs TREE1
#look for duplicates for TREE4 compared to TREE1
tree1_vs_tree4 = 1:length(temporary.4$uniquetree4)
tree1_vs_tree4 = temporary.4$uniquetree1==temporary.4$uniquetree4
temporary.4$tree1_vs_tree4 = tree1_vs_tree4

#this finds the duplicates in TREE4 and TREE1 for each entry
temporary.4[which(temporary.4$tree1_vs_tree4 == "TRUE"),]

#TREE4 vs TREE2
#look for duplicates for TREE4 compared to TREE2
tree4_vs_tree2 = 1:length(temporary.4$uniquetree4)
tree4_vs_tree2 = temporary.4$uniquetree4==temporary.4$uniquetree2
temporary.4$tree4_vs_tree2 = tree4_vs_tree2

#this finds the duplicates in TREE4 and TREE2 for each entry
temporary.4[which(temporary.4$tree4_vs_tree2 == "TRUE"),]


################################################################################
## CHECK IF THERE ARE DUPLICATE ENTRIES OF TREE DATA WITHIN THE WHOLE DATASET ##
################################################################################

#concatenate all uniquetree 1, 2, 3 and 4 information from the newIL into one vector
masteruniquetree = paste0(newIL$uniquetree1,newIL$uniquetree2,newIL$uniquetree3,newIL$uniquetree4)

newIL$masteruniquetree = masteruniquetree

uniquetemporary = newIL[,c("TRP","entry_id","reader_initials","hubtack_county","cornerid","masteruniquetree")]

#now check for duplicates in this list
uniquecount = unique(uniquetemporary$masteruniquetree)
length(uniquecount)
length(uniquecount) == length(uniquetemporary$masteruniquetree) #needs to say TRUE

# If the above says "FALSE" find the duplicated masteruniquetrees
uniquedups <-uniquetemporary[duplicated(uniquetemporary$masteruniquetree)|duplicated(uniquetemporary$masteruniquetree, fromLast=TRUE),]
uniquedups2 = uniquedups[order(uniquedups$masteruniquetree),] 
write.csv(uniquedups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/uniquetree_duplicates_v1.8-1_2.csv", row.names = FALSE)



#############################################
## Check that L1_treex + species match up ###
#############################################

#TREE1
library(plyr)
L1tree1.species <- ddply(newIL, .(newIL$species, newIL$L1_tree1), nrow)
L1tree1.species
write.csv(L1tree1.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree-species.csv", row.names = FALSE)
#check L1_tree1 and species1 that do not match
trees[which(trees$L1_tree1 == "blue ash"),]


#TREE2
L1tree2.species <- ddply(newIL, .(newIL$species2, newIL$L1_tree2), nrow)
L1tree2.species
write.csv(L1tree2.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree2-species.csv", row.names = FALSE)
#check L1_tree2 and species2 that do not match
trees[which(trees$L1_tree2 == "blue ash"),]


#TREE3
L1tree3.species <- ddply(newIL, .(newIL$species3, newIL$L1_tree3), nrow)
L1tree3.species
write.csv(L1tree3.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree3-species.csv", row.names = FALSE)
#check L1_tree3 and species3 that do not match
trees[which(trees$L1_tree3 == "blue ash"),]


#TREE4
L1tree4.species <- ddply(newIL, .(newIL$species4, newIL$L1_tree4), nrow)
L1tree4.species
write.csv(L1tree4.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree4-species.csv", row.names = FALSE)
#check L1_tree4 and species4 that do not match
trees[which(trees$L1_tree4 == "blue ash"),]


##############################################
## Check that L1_treex + L3_tree2 match up ###
##############################################

#TREE1
library(plyr)
L1tree1.L3tree1 <- ddply(newIL, .(newIL$L3_tree1, newIL$L1_tree1), nrow)
L1tree1.L3tree1
write.csv(L1tree1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree1-L3tree1_2.csv", row.names = FALSE)
#check L1_tree1 and L3_tree1 that do not match
trees[which(trees$L1_tree1 == "blue ash"),]


#TREE2
L1tree2.L3tree2 <- ddply(newIL, .(newIL$L3_tree2, newIL$L1_tree2), nrow)
L1tree2.L3tree2
write.csv(L1tree2.L3tree2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree2-L3tree2_2.csv", row.names = FALSE)
#check L1_tree2 and L3_tree2 that do not match
trees[which(trees$L1_tree2 == "blue ash"),]


#TREE3
L1tree3.L3tree3 <- ddply(newIL, .(newIL$L3_tree3, newIL$L1_tree3), nrow)
L1tree3.L3tree3
write.csv(L1tree3.L3tree3, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree3-L3tree3_2.csv", row.names = FALSE)
#check L1_tree3 and L3_tree3 that do not match
trees[which(trees$L1_tree3 == "blue ash"),]


#TREE4
L1tree4.L3tree4 <- ddply(newIL, .(newIL$L3_tree4, newIL$L1_tree4), nrow)
L1tree4.L3tree4
write.csv(L1tree4.L3tree4, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree4-L3tree4_2.csv", row.names = FALSE)
#check L1_tree4 and L3_tree4 that do not match
trees[which(trees$L1_tree4 == "blue ash"),]


######################################
### Check for Duplicate X,Y points ###
######################################


#Check for duplicates in the point x values
cornersPointXcount = unique(IL$x)
length(cornersPointXcount)
length(cornersPointXcount) == length(IL$x) #needs to say TRUE
#this says FALSE, but there is one corner that has the same xs, but differnt ys, so is okay.

# If the above says "FALSE" find the duplicate IL$x
xdups <-IL[duplicated(IL$x)|duplicated(IL$x, fromLast=TRUE),]
xdups
xdups2 = xdups[order(xdups$x),] 
write.csv(xdups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/x_duplicates.csv", row.names = FALSE)

#Check for duplicates in the point y values
cornersPointYcount = unique(IL$y)
length(cornersPointYcount)
length(cornersPointYcount) == length(IL$y) #needs to say TRUE

# If the above says "FALSE" find the duplicate IL$y
ydups <-IL[duplicated(IL$y)|duplicated(IL$y, fromLast=TRUE),]
ydups
ydups2 = ydups[order(ydups$y),] 
write.csv(ydups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/y_duplicates.csv", row.names = FALSE)



######################################
### Check for Duplicate entryIDs   ###
######################################

#there are corners which were not entered in MySQL because there was no data for the whole township or border
#and instead of entering each corner in MySQL, I entered them in the csv used for georeferencing only.
#all these entries have entry_id equal to 0 or NA.
#so need to remove all these entries first before comparing entry_ids

#remove entries with 0 and NA values in the entry_id
entryid.IL <- IL[!(IL$entry_id %in% c(0,NA)),]
#just for fun select only entries with 0 and NA for the entry_ids and make sure that the total for
#nonentryid.IL and entryid.IL equals the total entries for IL
nonentryid.IL <- IL[(IL$entry_id %in% c(0,NA)),]
#nonentryid.IL = 249, entryid.IL = 57062, IL = 57311. They match!
249+57062

unique.entryid = unique(entryid.IL$entry_id)
length(unique.entryid)
length(unique.entryid) == length(entryid.IL$entry_id) #needs to say TRUE


# If the above says "FALSE" find the duplicate entryid.IL$entry_id
entryid.dups <-entryid.IL[duplicated(entryid.IL$entry_id)|duplicated(entryid.IL$entry_id, fromLast=TRUE),]
entryid.dups2 = entryid.dups[order(entryid.dups$entry_id),] 
write.csv(entryid.dups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/entryid_duplicates.csv", row.names = FALSE)




################################################################################
#### Check that there are entries for all State, Surveyor, Year, County  #######
################################################################################
#can use code below, but may be easier to sort in excel and find if there are any missing at the bottom

rm(list=ls())
IL = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/ndilpls_v1.8-1.csv", header = TRUE, stringsAsFactors = FALSE)

#STATE
state = IL[order(IL$state),] 
View(state)

#sort descending
library(plyr)
state_desc = arrange(IL,desc(state))
View(state_desc) #look at the output to see if there are state in the first row

statecount = count(IL$state)
statecount

#SURVEYOR
surveyor = IL[order(IL$surveyor_name),] 
View(surveyor)

#sort descending
library(plyr)
surveyor_desc = arrange(IL,desc(surveyor_name))
View(surveyor_desc) #look at the output to see if there are surveyors in the first row

surveyorcount = count(IL$surveyor_name)
View(surveyorcount)

#YEAR
year = IL[order(IL$year),] 
View(year)

#sort descending
library(plyr)
year_desc = arrange(IL,desc(year))
View(year_desc) #look at the output to see if there are surveyors in the first row

yearcount = count(IL$year)
View(yearcount)

#COUNTY
county = IL[order(IL$hubtack_county),] 
View(county)

#sort descending
county_desc = arrange(IL,desc(hubtack_county))
View(county_desc) #look at the output to see if there are surveyors in the first row

countycount = count(IL$hubtack_county)
View(countycount)

#VERSION
version = IL[order(IL$version),] 
View(version)

#sort descending
version_desc = arrange(IL,desc(version))
View(version_desc) #look at the output to see if there are surveyors in the first row

versioncount = count(IL$version)
View(versioncount)



################################################################
#### Check that Water/Wet entries have no trees entered ########
################################################################

watercheck <- IL[(IL$water %in% c("Water","Wet","water","wet")),]
nonwatercheck <- IL[!(IL$water %in% c("Water","Wet","water","wet")),]
56649+662 # =57311. It matches!

write.csv(watercheck[order(watercheck$L3_tree1),], file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/water_check.csv", row.names = FALSE)



################################################################
#### Summary Code to Use for ReadMe File                ########
################################################################


readers = as.data.frame(sort(unique(IL$reader_initials)))

reader.table = as.data.frame(sort(table(IL$reader_initials)))
reader.table
#compare the names in reader.table to teh reader.table$names listed below.
#version IL 1.8 reader names
reader.table$names = c("Sam Pecoraro", "Jill Deines", "Christina Wiech", "Will Tintor", "Jaclyn Cooney", "Rebecca O'Neil", 
          "Margaret Corcoran", "Benjamin Foster", "Grace Saalman", "Andrew Muench", "Nicole Fantozzi", "Nikki Micelotta",
          "Claire Mattison", "Isaac Evans", "Annie Han", "Will Chronister", "Kate Augustine", "Garrett Blad", 
          "Michelle Mueller", "Hannah Legatzke", "Zoe Volenec", "Kaitlin Powers","Anna Levesque", "Jody Peters", 
          "Emily Mears", "Caitlin Broderick", "Kim Bauer", "Amanda Buerger", "Alec Helmke", "Erin Nguyen","Da Som Kim", 
          "Mariel Cuellar", "Marissa Kivi", "Quercus Hamlin", "Bridget Bruns")
reader.table

length(reader.table$Var1)
#35 students & Jody and Jill entered data for Illinois in version 1.8

#table of years sorted by year
year.table = as.data.frame(table(IL$year))
year.table
#table of years sorted by frequency
year.table2 = as.data.frame(sort(table(IL$year)))
year.table2

#plot of number of records surveyed in each year. MAKE SURE TO SAVE THIS PLOT IN THE RIGHT DIRECTORY
library(ggplot2)
year = ggplot(year.table, aes(Var1,Freq))+geom_bar(stat="identity")+theme_minimal()+ylab("Count")+theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
png(paste("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/Count of Surveyor Year.png", sep = " "),   height = 768, width=1024)
year
dev.off()


length(unique(IL$TRP))
#Data came from 559 townships for Illinois in version 1.8

TRP.table = as.data.frame(table(IL$TRP))
TRP = ggplot(TRP.table, aes(Var1,Freq))+geom_bar(stat="identity")+theme_minimal()+ylab("Count")+theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
TRP
hist(TRP.table$Freq, breaks = 100, xlim=c(1,115), xaxt='n',xlab = "Number of PLS Corners", main = "Histogram of TRP Corner \nMajority of Counts should be in the 107-110 range ",las=1)
axis(side=1, at=seq(0,115,5), labels=seq(0,115,5))

histinfo = hist(TRP.table$Freq, breaks = 100, xlim=range(1:115))
histinfo

#Histogram of the number of PLS corners for each township. MAKE SURE TO SAVE THE PLOT TO THE RIGHT DIRECTORY!
png(paste("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/TRP Corner Counts.png", sep = " "),   height = 768, width=1024)
hist(TRP.table$Freq, breaks = 100, xlim=c(1,115), xaxt='n',xlab = "Number of PLS Corners", main = "Histogram of TRP Corner \nMajority of Counts should be in the 107-110 range ",las=1)
axis(side=1, at=seq(0,115,5), labels=seq(0,115,5))
dev.off()



#gives a summary of the number of corners check out townships with min and max 
summary(TRP.table$Freq) 
#for IL version 1.8 shows 1 township has 3 corners and 1 township has 112 corners.  Find these townships
TRP.table[which(TRP.table$Freq == 3),]
#for IL version 1.8 the township with 3 corners is 26N10W2 - this is a sliver township on the border of Indiana. It is fine.
TRP.table[which(TRP.table$Freq == 112),]
#for IL version 1.8 the township with 112 corners is 17N11W2 - this is a strangely shapped township with an extra protrusion to the south which included
#extra corners.  It is fine.

#number of townships added for version 1.8
v1.8 = table(IL$TRP,IL$version)
v1.8db = as.data.frame(v1.8)
View(v1.8db)
colnames(v1.8db) = c("TRP", "version","corners")
head(v1.8db)

sorted = v1.8db[order(v1.8db$version,v1.8db$corners),]
head(sorted)
tail(sorted)

sort2 <- subset(sorted, corners > 0, select=c(TRP,version,corners))
View(sort2)

unique(sort2$version) 
#unique version values: 0, 1.6, 1.7, 1.8

version1.8 = subset(sort2,version == 1.8, select=c(TRP,version,corners))
View(version1.8[order(version1.8$TRP),])
version1.8$TRP
#24 version 1.8-1 townships


version1.7 = subset(sort2,version == 1.7, select=c(TRP,version,corners))
View(version1.7)
version1.7$TRP
#108 version 1.7 townships. But 2 of them 5N12W2 and 6N11W2 were originally entered in version 1.6, so 
#version 1.7 count is 106

version1.6 = subset(sort2,version == 1.6, select=c(TRP,version,corners))
View(version1.6)
version1.6$TRP
#114 version 1.7 townships

version0 = subset(sort2,version == 0, select=c(TRP,version,corners))
View(version0)
version0$TRP
#315 version 0 townships

#The total number of townships in the version 1.8 data is 559. 
#But if you add up the number of townships in each individual version
#v1.8=23, v1.7=109, v1.6=114, v0=315...  315+114+109+23 = 561
#there are 2 additional townships (6N11W2 and 5N12W2) when you add up the invidual township 561 vs 559.
# 6N11W2 was entered in v1.6 and 1.7. 
#Most of the corner notes were missing for township 6N11W2. But there were a few available when entered for v1.6
#Then when entering notes for v1.7 we found the S border notes for 6N11W2 and entered those.
# 5N12W2 had the township corner 700700 entered in v1.7 and the rest of the township was entered in v1.6
#the 700700 corner was entered as no data because we had all 3 bordering townships entered in v1.7 and 
#found that there was no data for 5N12W2 corner 700700 in any of the 3 townships.

#################################################################
#### TRP check compared between GIS map and ndilpls_v1.8 file ###
#################################################################

rm(list=ls())

#find the duplicated TRP
#the file you bring in should have all the TRPs from both GIS and ndilpls in the same column.
#So the file should have a column for source (ndilpls or GIS), version, TRP
TRPndilpls_GIScheck = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/TRPndilpls_GISmapCheck.csv", header = TRUE, stringsAsFactors = FALSE)
uniquedups <-TRPndilpls_GIScheck[duplicated(TRPndilpls_GIScheck$TRP)|duplicated(TRPndilpls_GIScheck$TRP, fromLast=TRUE),]
uniquedups2 = uniquedups[order(uniquedups$TRP),] 
write.csv(uniquedups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/TRP_duplicates.csv", row.names = FALSE)

#find the TRPs that are not in ndilpls or not in GIS
#the file you bring in should have separate columns for the ndilplsTRPs and the GISTRPs
TRP_notdups = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/TRP_notduplicates.csv", header = TRUE, stringsAsFactors = FALSE)
NotInGIS = TRP_notdups$ndilpls_TRP[!(TRP_notdups$ndilpls_TRP %in% TRP_notdups$GIS_TRP)]
NotInGIS
#This returned 40N1E3 and 39N1E3 which are in ndilpls but not in GIS. That is because the corners in this township are the west border corners and
#are actually the east border of 22N11E4 in Ogle county.  So this is okay.
#18N3E3 was also returned as being in ndilpls and not on the GIS map.  This had been marked on the map as 18N3E4. Corrected on GIS map.


NotInndilpls = TRP_notdups$GIS_TRP[!(TRP_notdups$GIS_TRP %in% TRP_notdups$ndilpls_TRP)]
NotInndilpls







