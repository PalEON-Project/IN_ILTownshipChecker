############################################
######## Georeferencing MySQL Data #########
############################################

#created by Jody Peters

setwd("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing")
rm(list=ls())


###########################################
##### Corner Locations from GIS dbf #######
###########################################
#read in the dbf file from GIS with the x,y coordinates for the corner locations
library(foreign)
dbf = read.dbf("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/Illinois/Projected/IL v1.8 Georef/v1.8ILINSouth_corners.dbf", as.is = TRUE)
head(dbf)


#select only the variables: PointX, PointY, TRP, cornerid, state
#save as a new csv and read in the new csv
temporary <- dbf[,c("State", "TRP", "cornerid", "POINT_X", "POINT_Y")]
head(temporary)

#create the uniqueID column which is TRP and cornerid concatenated - use this to join the 
#pointx/pointy data to the MySQL tree data
uniqueID = paste0(temporary$TRP,temporary$cornerid)
uniqueID = paste0(temporary$State,temporary$TRP,temporary$cornerid) #include State in uniqueID because the v1.8ILINSouth corners was in both states

#add uniqueID column to the corners data frame
temporary$uniqueID = uniqueID
head(temporary)
list(names(temporary))

write.csv(temporary, file = "C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/ILINSouthGISXY.csv", row.names = FALSE)

options(digits=11)
corners = read.csv("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/ILINSouthGISXY.csv", stringsAsFactors = FALSE, header=TRUE)
head(corners)



################################################################
#########  Township Group from MySQL Exported Data #############
################################################################

rm(list=ls())

#read in the township group csv file
m = read.csv("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/Illinois/Projected/IL v1.8 Georef/IL v1.8 townships/township groups/South Corners.csv", stringsAsFactors = FALSE, header = TRUE)

#Add column for version number
version = rep(1.8,1101)  #repeat version number 1.8, MAKE SURE YOU PUT IN THE CORRECT NUMBER OF ROWS
m$version = version

#create the TRP (township + range + meridian concatenated
#and uniqueID (TRP + cornerid concatenated) columns 
#use the uniqueID column to join the pointx/pointy data to the MySQL tree data
TRP = paste0(m$township,m$townshipdir,m$rangenum,m$rangedir,m$meridian)
m$TRP = TRP #add the TRP column to the m database
head(m)

TRPcount = unique(m$TRP)
length(TRPcount) #check the number of townships to the number in the Lookup Groups number of townships in this group
list(TRPcount) #check this list against the Lookup Groups list of townships in this group


uniqueID = paste0(m$TRP,m$cornerid)
uniqueID = paste0(m$state,m$TRP,m$cornerid)#include State in uniqueID because the v1.8ILINSouth corners was in both states
m$uniqueID = uniqueID #add the uniqueID column to the m database

write.csv(m, file = "C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/SouthMySQL.csv", row.names = FALSE)

#in the SQL.csv file - move uniqueID, TRP and version columns to the beginning of the file then read the file back in.

mysql = read.csv("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/SouthMySQL.csv", stringsAsFactors = FALSE, header=TRUE)



##### CHECK FOR DUPLICATES IN UNIQUEIDS IN THE mysql DATAFRAME AND IN THE corners DATAFRAME #######
#check to see how many unique IDs you have and in the mysql dataframe if this equals the length
#of the uniqueID column. If it doesn't you have a duplicate uniqueID.
uniqueIDcount = unique(mysql$uniqueID)
length(uniqueIDcount)
length(uniqueIDcount) == length(mysql$uniqueID)
#### IF THIS EQUALS FALSE YOU HAVE A UNIQUEID THAT IS DUPLICATED!!!######

######################## FIX THIS PROBLEM! Use Code Below ##############################
# find the duplicate uniqueIDs
dups <-mysql[duplicated(mysql$uniqueID)|duplicated(mysql$uniqueID, fromLast=TRUE),]
dups

#this returns a table with the uniqueID label and how many times it is duplicated.
table(dups$uniqueID)
#go back to MySQL and fix this, also fix in the township group csv.
#then re-run the code back to this point 
#and make sure length(uniqueIDcount) == length(m$uniqueID) is TRUE


################

#check to see how many unique IDs you have in the corners dataframe and if this equals the 
#length of the uniqueID column. If it doesn't, you have a duplicate uniqueID.
cornersuniqueIDcount = unique(corners$uniqueID)
length(cornersuniqueIDcount)
length(cornersuniqueIDcount) == length(corners$uniqueID)
#### IF THIS EQUALS FALSE YOU HAVE A CORNERS UNIQUEID THAT IS DUPLICATED!!!######

######################## FIX THIS PROBLEM! Use Code Below##############################
# find the duplicate uniqueIDs
cornersdups <-corners[duplicated(corners$uniqueID)|duplicated(corners$uniqueID, fromLast=TRUE),]
cornersdups

#this returns a table with the uniqueID label and how many times it is duplicated.
table(cornersdups$uniqueID)
#go back to GIS and fix this.
#then re-upload the dbf file and re-run the code back to this point 
#and make sure length(cornersuniqueIDcount) == length(corners$uniqueID) is TRUE




#### NOW COMPARE THE UNIQUEIDS IN THE 2 DATAFRAMES AGAINST EACH OTHER ####

#check that the uniqueIDs in the corners and mysql dataframes match
Cunique = sort(corners$uniqueID)
munique = sort(mysql$uniqueID)

corner_notmysql = as.data.frame(setdiff(Cunique,munique)) #this tells you what uniqueIDs are in the corners dataframe, but not the mysql dataframe
colnames(corner_notmysql)= c("in GIS_not in mysql")
corner_notmysql
mysql_notcorner = as.data.frame(setdiff(munique,Cunique)) #this tells you what uniqueIDs are in the mysql dataframe, but not the corners dataframe
colnames(mysql_notcorner) = c("in mysql_not in GIS")
mysql_notcorner
#IF THERE ARE UNIQUEIDS THAT COME UP IN ONE DATASET BUT NOT THE OTHER FIX IT!!

#Check for duplicates in the pointx and pointy values
cornersPointXcount = unique(corners$PointX)
length(cornersPointXcount)
length(cornersPointXcount) == length(corners$PointX) #needs to say TRUE

cornersPointYcount = unique(corners$PointY)
length(cornersPointYcount)
length(cornersPointYcount) == length(corners$PointY) #needs to say TRUE



#create new dataframe from the corners dataframe with just the uniqueID, pointx and pointy
cornersxy = corners[,c("uniqueID", "POINT_X", "POINT_Y")]
head(cornersxy)


#Merge mysql dataframe and xy dataframe based on uniqueID

xymerge = merge(cornersxy,mysql,by = "uniqueID")
head(xymerge[,1:10])
head(xymerge[,71:81])

list(names(xymerge))
table(xymerge$year)

#Check Township corners (700100,700700 and 100100) for any with No Data.  You do not want Township corners with No Data, 
#just Township corners with No tree or verbatim trees listed.
TownshipCheck = xymerge[which(xymerge$typecorner == "Township"),] #select just the Township corners
#create dataframe from the Township corners with the entryid, uniqueID, typecorner, no data, no tree, verbatim, and general notes columns
TownshipOutput = data.frame(TownshipCheck$entry_id, TownshipCheck$uniqueID, TownshipCheck$typecorner, TownshipCheck$nodata, TownshipCheck$notree, TownshipCheck$verbatim, TownshipCheck$generalnotes)
View(TownshipOutput) #view as a table.  If there are entries with "No data", then go to the merge.csv and remove. 


#remove Null entries
removeNULLs = xymerge
removeNULLs[removeNULLs == "NULL"] <- ""
View(removeNULLs)


#sort by nodata, notree, waterwet, verbatim, ecotype, feature, and gneralnotes then save as a csv
library(dplyr)
nodata_tree_sort = arrange(removeNULLs, desc(nodata), desc(notree), desc(waterwet), verbatim, desc(ecotype), desc(feature), desc(generalnotes))
View(nodata_tree_sort)



#save the file - it should have the NULLs removed and be sorted by nodata, no tree, waterwet, etc
write.csv(nodata_tree_sort, file = "C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/SouthGIS_MySQL_merge.csv", row.names = FALSE)
# CHECK THIS NEW CSV THAT THE NO TREE, NO DATA AND WATER/WET ENTRIES ARE CORRECT AND THAT THERE ARE NO
# BLANKS IN THE VERBATIM COLUMN (THESE MAY BE CORNERS THAT PEOPLE FORGOT TO MARK NO TREE). 
# IF THE CSV WATER/NO TREE/NO DATA NOTES ARE NOT CORRECT, CORRECT THEM IN THE CSV & IN MYSQL AND SAVE.

# While you have the CSV open, 1) rename the waterwet column and move to after the notrees column and 
# 2) add columns L1_treeX and L3_treeX for trees 1, 2, 3, and 4.

rm(list=ls())


# Now that you have checked the CSV above to make sure the No data, No tree, and Waters are correct, 
# read the csv back in to R
merge = read.csv("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/SouthGIS_MySQL_merge.csv", stringsAsFactors = FALSE, header=TRUE)

list(names(merge))

#clear leading and trailing spaces for tree 1
merge$verbatim <- gsub('^[ \t]+|[ \t]+$', '', merge$verbatim)
merge$verbatim <- gsub('[ ]{2}', ' ', merge$verbatim)

#Puts all the verbatim trees in the L1_tree1 column
for(i in 1:length(merge$L1_tree1)){
  merge$L1_tree1[i]=merge$verbatim[i]
}


#This loops puts the No data, No tree, Water and Wet labels into the L1_tree1 column
for(i in 1:length(merge$L1_tree1)){
  if(merge$nodata[i] == 'yes'){
    merge$L1_tree1[i] <- 'No data'
  }
  else if(merge$notree[i] == 'yes' & merge$water[i] == 'Water'){
    merge$L1_tree1[i] <- 'Water'
  }
  else if(merge$notree[i] == 'yes' & merge$water[i] == 'Wet'){
    merge$L1_tree1[i] <- 'Wet'
  }
  else if(merge$notree[i] == 'yes' & merge$water[i] == ''){
    merge$L1_tree1[i] <- 'No tree'
  }
}

table(merge$L1_tree1)

write.csv(merge, file = "C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/SouthGIS_MySQL_Nodata_tree_water.csv", row.names = FALSE)

#Entering the Level 1 and Level 3 trees
rm(list=ls())
#Load the data
library(dplyr)
trees = read.csv("C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/SouthGIS_MySQL_Nodata_tree_water.csv",stringsAsFactors = FALSE, header = TRUE)
colnames(trees)

#Load the lookup table
lookup = read.csv("./R Code/fullpaleonconversion_v0.4-2.csv", header = TRUE, stringsAsFactors = FALSE)
head(lookup)
unique(lookup$Level_3a)


##################
#TREE1 - CONVERTING from Level 1 taxa to Level 3 taxa
#check to see if there are any dittos or strange names in the Level 1 trees (L1_tree1) that will cause issues when converting to Level 3 trees
unique(trees$L1_tree1)
#use this code to fid entries with L1_tree1 that you want to check
trees[which(trees$L1_tree1 == "precon"),]

#this is the lookup table to enter PalEON Level 3a taxa into the L3_tree1 column given the entry in the L1_tree1 column
for(i in 1:length(trees$L3_tree1)){
      trees$L3_tree1[i] = lookup[match(trees$L1_tree1[i], lookup$Level_1, nomatch = NA),2]
}

unique(trees$L3_tree1) #this let's you know if there are any trees that were not converted to Level 3 - look for the "NA"s
tree1.table = as.data.frame(table(trees$L3_tree1))
tree1.table
sum(tree1.table[,2]) #Make sure this sum equals the number of observations in the tree1 object below
#if there are NAs for L3_tree1 - find out what the corresponding L1_tree1 is
tree1 = data.frame(cbind(trees$L1_tree1,trees$L3_tree1), stringsAsFactors = FALSE)
colnames(tree1) = c("L1_tree1","L3_tree1")
new_tree1 <- tree1[is.na(tree1$L3_tree1),] #this finds if there are any NA L3trees and the corresponding L1tree
unique(new_tree1$L1_tree1) 
#1 enter these tree names in the fullpaleonconversion.csv reload the lookup table with the new additions
lookup = read.csv("./R Code/fullpaleonconversion_v0.4-2.csv", header = TRUE, stringsAsFactors = FALSE)
#2 re-run the L3_tree1 loop again 
#3 make sure there are no NAs in the L3_tree1 table 
#4 and check that the sum of tree1.table value is the same as the number of observations in the tree1 object

##################
#TREE2 - CONVERTING from Level 1 taxa to Level 3 taxa
#clear leading and trailing spaces for tree 2
trees$verbatim2 <- gsub('^[ \t]+|[ \t]+$', '', trees$verbatim2)
trees$verbatim2 <- gsub('[ ]{2}', ' ', trees$verbatim2)

#check to see if there are any dittos or strange names in the verbatim2 trees that will cause issues when converting to Level 1 trees
unique(trees$verbatim2)
#move the verbatim2 tree names to the L1_tree2 column
trees$L1_tree2 = trees$verbatim2
#check to see if the trees and counts listed in L1_tree2 are the same as those in the verbatim2
v = as.data.frame(table(trees$verbatim2))
colnames(v) = c("verbatim2","verbFreq")
L1 = as.data.frame(table(trees$L1_tree2))
colnames(L1) = c("L1_tree2","L1t2Freq")
total = cbind(v,L1) 
total #check this table to make sure names and counts are the same for verbatim2 and L1_tree2 

#Use this loop to change any dittos, do, "", anothers in the L1_tree2 column to the tree name in the L1_tree1 column
#figure out what needs to be entered in here from the unique(trees$verbatim2) above
for(i in 1:length(trees$L1_tree2)){
  if(trees$L1_tree2[i] == "another"){
    trees$L1_tree2[i] <- trees$L1_tree1[i]
  }
}


#code to use when changing multiple dittos, do
for(i in 1:length(trees$L1_tree2)){
  if(trees$L1_tree2[i] == "do"){
    trees$L1_tree2[i] <- trees$L1_tree1[i]
  }
  if(trees$L1_tree2[i] == "Do"){
    trees$L1_tree2[i] <- trees$L1_tree1[i]
  }
  if(trees$L1_tree2[i] == "DO"){
    trees$L1_tree2[i] <- trees$L1_tree1[i]
  }
}



#double checks to see if the dittos/anothers/" are changed in the L1_tree2 column
unique(trees$L1_tree2)
L1 = as.data.frame(table(trees$L1_tree2))
L1
sum(L1$Freq)

#Now convert L1_tree2 trees to L3_tree2 trees

#this is the lookup table to enter PalEON Level 3a taxa into the L3_tree2 column given the entry in the L1_tree2 column
for(i in 1:length(trees$L3_tree2)){
  trees$L3_tree2[i] = lookup[match(trees$L1_tree2[i], lookup$Level_1, nomatch = NA),2]
}


unique(trees$L3_tree2) #this let's you know if there are any trees that were not converted to Level 3 - look for the "NA"s
tree2.table = as.data.frame(table(trees$L3_tree2))
sum(tree2.table[,2]) #Make sure this sum equals the number of observations in the tree2 object below
#if there are NAs for L3_tree2 - find out what the corresponding L1_tree2 is
tree2 = data.frame(cbind(trees$L1_tree2,trees$L3_tree2), stringsAsFactors = FALSE)
colnames(tree2) = c("L1_tree2","L3_tree2")
new_tree2 <- tree2[is.na(tree2$L3_tree2),] #this finds if there are any NA L3trees and the corresponding L1tree
unique(new_tree2$L1_tree2)
#if you want to find a specific row with a certain tree name that needs to be double checked in MySQL use
#where 'blak oak' is the weird tree name you want to check
trees[which(trees$L1_tree2 == "beechy"),]

#Back to what to do with the results from the unique(new_tree2$L1_tree2)
unique(new_tree2$L1_tree2)   
#1 If only "" is returned do this:
length(new_tree2$L3_tree2)+sum(tree2.table[,2]) #make sure this equals the total number of records
#2 If additional trees show up in unique(new_tree2$L1_tree2) - enter these tree names in the fullpaleonconversion.csv reload the lookup table with the new additions
lookup = read.csv("./R Code/fullpaleonconversion_v0.4-2.csv", header = TRUE, stringsAsFactors = FALSE)
#3 re-run the L3_tree2 loop again 
#4 make sure there are no NAs in the L3_tree2 table 
#5 and check that the sum of tree2.table value is the same as the number of observations in the tree2 object


##################
#TREE3 - CONVERTING from Level 1 taxa to Level 3 taxa
#clear leading and trailing spaces for tree 3
trees$verbatim3 <- gsub('^[ \t]+|[ \t]+$', '', trees$verbatim3)
trees$verbatim3 <- gsub('[ ]{2}', ' ', trees$verbatim3)

#check to see if there are any dittos or strange names in the verbatim3 trees that will cause issues when converting to Level 1 trees
unique(trees$verbatim3)
#move the verbatim3 tree names to the L1_tree3 column
trees$L1_tree3 = trees$verbatim3
#check to see if the trees and counts listed in L1_tree3 are the same as those in the verbatim3
v = as.data.frame(table(trees$verbatim3))
colnames(v) = c("verbatim3","verbFreq")
L1 = as.data.frame(table(trees$L1_tree3))
colnames(L1) = c("L1_tree3","L1t3Freq")
total = cbind(v,L1) 
total #check this table to make sure names and counts are the same for verbatim3 and L1_tree3

#Use this loop to change any dittos, do, "", anothers in the L1_tree3 column to the tree name in the L1_tree2 column
#figure out what needs to be entered in here from the unique(trees$verbatim3) above
for(i in 1:length(trees$L1_tree3)){
  if(trees$L1_tree3[i] == "another"){
    trees$L1_tree3[i] <- trees$L1_tree2[i]
  }
}

#double checks to see if the dittos/anothers/" are changed in the L1_tree3 column
unique(trees$L1_tree3)
L1 = as.data.frame(table(trees$L1_tree3))
L1
sum(L1$Freq)

#Now convert L1_tree3 trees to L3_tree3 trees

#this is the lookup table to enter PalEON Level 3a taxa into the L3_tree3 column given the entry in the L1_tree3 column
for(i in 1:length(trees$L3_tree3)){
  trees$L3_tree3[i] = lookup[match(trees$L1_tree3[i], lookup$Level_1, nomatch = NA),2]
}


unique(trees$L3_tree3) #this let's you know if there are any trees that were not converted to Level 3 - look for the "NA"s
tree3.table = as.data.frame(table(trees$L3_tree3))
sum(tree3.table[,2]) #Make sure this sum equals the number of observations in the tree3 object below
#if there are NAs for L3_tree3 - find out what the corresponding L1_tree3 is
tree3 = data.frame(cbind(trees$L1_tree3,trees$L3_tree3), stringsAsFactors = FALSE)
colnames(tree3) = c("L1_tree3","L3_tree3")
new_tree3 <- tree3[is.na(tree3$L3_tree3),] #this finds if there are any NA L3trees and the corresponding L1tree
unique(new_tree3$L1_tree3)   
#if you want to find a specific row with a certain tree name that needs to be double checked in MySQL use
#where 'blak oak' is the weird tree name you want to check
trees[which(trees$L1_tree3 == "blak oak"),]

#Back to what to do with the results from the unique(new_tree3$L1_tree3)
unique(new_tree3$L1_tree3)   
#1 If only "" is returned do this:
length(new_tree3$L3_tree3)+sum(tree3.table[,2]) #make sure this equals the total number of records
#2 If additional trees show up in unique(new_tree2$L1_tree2) - enter these tree names in the fullpaleonconversion.csv reload the lookup table with the new additions
lookup = read.csv("./R Code/fullpaleonconversion_v0.4-2.csv", header = TRUE, stringsAsFactors = FALSE)
#3 re-run the L3_tree2 loop again 
#4 make sure there are no NAs in the L3_tree2 table 
#5 and check that the sum of tree2.table value is the same as the number of observations in the tree2 object


##################
#TREE4 - CONVERTING from Level 1 taxa to Level 3 taxa
#clear leading and trailing spaces for tree 4
trees$verbatim4 <- gsub('^[ \t]+|[ \t]+$', '', trees$verbatim4)
trees$verbatim4 <- gsub('[ ]{2}', ' ', trees$verbatim4)

#check to see if there are any dittos or strange names in the verbatim4 trees that will cause issues when converting to Level 1 trees
unique(trees$verbatim4)
#move the verbatim4 tree names to the L1_tree4 column
trees$L1_tree4 = trees$verbatim4
#check to see if the trees and counts listed in L1_tree4 are the same as those in the verbatim4
v = as.data.frame(table(trees$verbatim4))
colnames(v) = c("verbatim4","verbFreq")
L1 = as.data.frame(table(trees$L1_tree4))
colnames(L1) = c("L1_tree4","L1t4Freq")
total = cbind(v,L1) 
total #check this table to make sure names and counts are the same for verbatim24and L1_tree4

#Use this loop to change any dittos, do, "", anothers in the L1_tree4 column to the tree name in the L1_tree3 column
#figure out what needs to be entered in here from the unique(trees$verbatim4) above
for(i in 1:length(trees$L1_tree4)){
  if(trees$L1_tree4[i] == "another"){
    trees$L1_tree4[i] <- trees$L1_tree4[i]
  }
}


#double checks to see if the dittos/anothers/" are changed in the L1_tree4 column
unique(trees$L1_tree4)
L1 = as.data.frame(table(trees$L1_tree4))
L1
sum(L1$Freq)

#Now convert L1_tree4 trees to L3_tree4 trees

#this is the lookup table to enter PalEON Level 3a taxa into the L3_tree4 column given the entry in the L1_tree4 column
for(i in 1:length(trees$L3_tree4)){
  trees$L3_tree4[i] = lookup[match(trees$L1_tree4[i], lookup$Level_1, nomatch = NA),2]
}


unique(trees$L3_tree4) #this let's you know if there are any trees that were not converted to Level 3 - look for the "NA"s
tree4.table = as.data.frame(table(trees$L3_tree4))
sum(tree4.table[,2]) #Make sure this sum equals the number of observations in the tree4 object below
#if there are NAs for L3_tree4 - find out what the corresponding L1_tree4 is
tree4 = data.frame(cbind(trees$L1_tree4,trees$L3_tree4), stringsAsFactors = FALSE)
colnames(tree4) = c("L1_tree4","L3_tree4")
new_tree4 <- tree4[is.na(tree4$L3_tree4),] #this finds if there are any NA L3trees and the corresponding L1tree
unique(new_tree4$L1_tree4)   
#if you want to find a specific row with a certain tree name that needs to be double checked in MySQL use
#where 'blak oak' is the weird tree name you want to check
trees[which(trees$L1_tree4 == "blak oak"),]

#Back to what to do with the results from the unique(new_tree2$L1_tree2)
unique(new_tree4$L1_tree4)   
#1 If only "" is returned do this:
length(new_tree4$L3_tree4)+sum(tree4.table[,2]) #make sure this equals the total number of records
#2 If additional trees show up in unique(new_tree2$L1_tree2) - enter these tree names in the fullpaleonconversion.csv reload the lookup table with the new additions
lookup = read.csv("./R Code/fullpaleonconversion_v0.4-2.csv", header = TRUE, stringsAsFactors = FALSE)
#3 re-run the L3_tree2 loop again 
#4 make sure there are no NAs in the L3_tree2 table 
#5 and check that the sum of tree2.table value is the same as the number of observations in the tree2 object

##################
# Add NAs to all L1_treeX 
##################
trees$L1_tree2 = gsub("^$|^ $", "NA", trees$L1_tree2)
trees$L1_tree3 = gsub("^$|^ $", "NA", trees$L1_tree3)
trees$L1_tree4 = gsub("^$|^ $", "NA", trees$L1_tree4)


##################
#CREATE TABLES TO DOUBLE CHECK NUMBERS FOR TRP, L1/L3 TREES, DIAMETERS, DEGREES, AND CHAINSTREES
####################

#TRP table - check that the number of townships is the same as what we have in the Lookup Groups excel file
#and check that the number of corners in each township makes sense (107-110, unless it is a funky township)
as.data.frame(table(trees$TRP))

####################
#L1 and L3 counts for trees 1-4
library(plyr)
counts.tree1 <- ddply(trees, .(trees$L3_tree1, trees$L1_tree1), nrow)
counts.tree1
sum(counts.tree1[,3]) #check that this equals the number of observations of trees

counts.tree2 <- ddply(trees, .(trees$L3_tree2, trees$L1_tree2), nrow)
counts.tree2
sum(counts.tree2[,3]) #check that this equals the number of observations of trees

counts.tree3 <- ddply(trees, .(trees$L3_tree3, trees$L1_tree3), nrow)
counts.tree3
sum(counts.tree3[,3]) #check that this equals the number of observations of trees

counts.tree4 <- ddply(trees, .(trees$L3_tree4, trees$L1_tree4), nrow)
counts.tree4
sum(counts.tree4[,3]) #check that this equals the number of observations of trees

####################
#diameter counts
#1. Run the code below for each tree diameter. 
#2. Then if there are any entries over 60 inches in diameters, use this to find them and double check that they have been double checked
trees[which(trees$diameter == "60"),]

#tree1 diameter
diameter1 = as.data.frame(table(trees$diameter)) #scroll through the diameters looking for small or large weird values.
diameter1
sum(diameter1[,2])+sum(is.na(trees$diameter)) #check this equals number of observations of trees

#tree2 diameter
diameter2 = as.data.frame(table(trees$diameter2)) #scroll through the diameters looking for small or large weird values.
diameter2
sum(diameter2[,2])+sum(is.na(trees$diameter2)) #check this equals number of observations of trees

#tree3 diameter
diameter3 = as.data.frame(table(trees$diameter3)) #scroll through the diameters looking for small or large weird values.
diameter3
sum(diameter3[,2])+sum(is.na(trees$diameter3)) #check this equals number of observations of trees

#tree4 diameter
diameter4 = as.data.frame(table(trees$diameter4)) #scroll through the diameters looking for small or large weird values.
diameter4
sum(diameter4[,2])+sum(is.na(trees$diameter4)) #check this equals number of observations of trees

####################
#degree counts
#if there are any entries over 90 degrees, use this to find them and double check that they have been double checked
trees[which(trees$degrees2 == "150"),]

#tree1 degree
degree1 = as.data.frame(table(trees$degrees)) #scroll through the degrees looking for values over 90 or weird values.
degree1
sum(degree1[,2])+sum(is.na(trees$degrees)) #check this equals number of observations of trees

#tree2 degree
degree2 = as.data.frame(table(trees$degrees2)) #scroll through the degrees looking for values over 90 or weird values.
degree2
sum(degree2[,2])+sum(is.na(trees$degrees2)) #check this equals number of observations of trees

#tree3 degree
degree3 = as.data.frame(table(trees$degrees3)) #scroll through the degrees looking for values over 90 or weird values.
degree3
sum(degree3[,2])+sum(is.na(trees$degrees3)) #check this equals number of observations of trees

#tree4 degree
degree4 = as.data.frame(table(trees$degrees4)) #scroll through the degrees looking for values over 90 or weird values.
degree4
sum(degree4[,2])+sum(is.na(trees$degrees4)) #check this equals number of observations of trees

####################
#chainstree(links) counts
#if there are any entries you want to check, use this to find them and double check that they have been double checked
trees[which(trees$chainstree == "22.85"),]

#tree1 chainstree
chainstree1 = as.data.frame(table(trees$chainstree)) #scroll through the degrees looking for values over 90 or weird values.
chainstree1
sum(chainstree1[,2])+sum(is.na(trees$chainstree)) #check this equals number of observations of trees

#use this if you have to fix some of the chainstrees that were not converted to links.
trees[1154,41] = 2285
trees[1154,51] = 1675
trees[735,41] = 145

#tree2 chainstree
chainstree2 = as.data.frame(table(trees$chainstree2)) #scroll through the degrees looking for values over 90 or weird values.
chainstree2
sum(chainstree2[,2])+sum(is.na(trees$chainstree2)) #check this equals number of observations of trees

#tree3 chainstree
chainstree3 = as.data.frame(table(trees$chainstree3)) #scroll through the degrees looking for values over 90 or weird values.
chainstree3
sum(chainstree3[,2])+sum(is.na(trees$chainstree3)) #check this equals number of observations of trees

#tree4 chainstree
chainstree4 = as.data.frame(table(trees$chainstree4)) #scroll through the degrees looking for values over 90 or weird values.
chainstree4
sum(chainstree4[,2])+sum(is.na(trees$chainstree4)) #check this equals number of observations of trees


# Once you have checked the TRP, L1/L3 trees, diameters, degrees, and chainstrees, save the file 
# The [,-1] removes the UniqueID column.  
# You'll need to change the order of the columns to match all the other Georef files that create the Group_trees shapefiles
# Columns to move: TRP and version to column 1 and 3, change Point_X and Point_Y labels to x and y, clear the timestamp,
# move nodata, notree, and water in between ecotypenotes3 and feature columns with the order being water, nodata then no tree
write.csv(trees[,-1], file = "C:/Users/jmurray7/Dropbox/GIS PalEON/Section Georeferencing/R Code/SouthIL_IN/ILINSouth_Georef.csv", row.names = FALSE)


