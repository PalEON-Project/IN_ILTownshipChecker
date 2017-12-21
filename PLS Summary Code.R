###############################################################################
######## PLS Summary - USE RESULTS FOR QA/QC and for IN/IL PLS READMEs ########
###############################################################################

#created by Jody Peters September/October 2017 to do a final QA/QC of the georferenced PLS data before
#posting it on the wiki/GitHUb and to get a summary of new townships entered in the Notre Dame database
#also use this to help update the ndil[in]pls_vXX README files


#QA/QC needs:
#1. Make sure all No tree, No data, Water, Wet have NA values in their diameter, degrees, chainstree columns for all 4 trees
#2. Check for duplicate tree entries
#3. check that L1_treex + species match up
#4. check that L3_treex + L1_treex match up and use this to update the paleonconversion csv on GitHub
#5. check for buckhorns (or any other weird tree names) when checking L1trees. If any go back and check those.
    #IN does have a buckhorn in the notes JP checked 10-18-17
#6. check for duplicate x/y points and entryids
#7. check that there are entries for all state, surveyor, year, county 
#8. check that Water/Wet entries have no trees entered
#9. check that the TRPs listed in the data file are the same ones listed in the GIS layers


#extra code in case you want to find a row with specific value in a column
IL[which(IL$entry_id == 679872),]

setwd("C:/Users/jmurray7/Dropbox/GIS PalEON/IL_IN_WI Unprojected")
rm(list=ls())

#BEFORE READING IN THE CSVS: do a sort by L1_tree1 and make sure that all No tree, No data, Water, Wet have
#NA for their diamters, degrees, and chainstrees and make sure that No tree, No data, Water and Wet are capitalized

#labeled both IL and IN files as "state" object so that I didn't need to create code for each state separately.
#I originally had the code read: IL = read.csv(ndilpls_vX.X.csv") or IN = read.csv(ndinpls_vX.X.csv"), but switched to the following
state = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/ndilpls_v1.8-1.csv", header = TRUE, stringsAsFactors = FALSE)
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)


#57311 records in Illinois version 1.8-1
#57065 records in Indiana version 1.7

#remove Water, Wet, No data, and No tree entries so only entries with trees are included in the new dataframe
newstate <- state[!(state$L3_tree1 %in% c("Water","Wet","No data", "No tree")),]
#just for fun select only the Water, Wet, No data and No tree entries and make sure that the total for
#nontreestate and newstate equals the total entries for each state
nontreestate <- state[(state$L3_tree1 %in% c("Water","Wet","No data", "No tree")),]
#ILv1.8-1: nontreeIL = 29003, newIL = 28308, IL = 57311. They match!
#INv1.7: nontreeIN = 7602, newIN = 49463, IN = 57065. They match!

#get a table of the counts of water, wet, no tree and no data
library(dplyr)
nontreecounts = state %>% group_by(L3_tree1) %>% tally()
View(nontreecounts)
#In IN v1.7: 689 No data, 5595 No tree, 1295 Water and 23 Wet entries


#sort newIL by trees to make sure all entries have trees in tree1 column
#sort ascending
newstatesort = newstate[order(newstate$verbatim),] 
View(newstatesort) #look at the output to see if there are trees in the first row
#added this code for IN because there were some corners from Morton's records that weren't entered in our MySQL database
#but came as a spreadsheet from Morton and the spreadsheet didn't have all the column headings (e.g., species, speciescode)
#as if the data had been entered into MySQL. So wanted to sort by L1_tree1 because that was brought in from the Morton data.
newstatesort = newstate[order(newstate$L1_tree1),] 
View(newstatesort) #look


#sort descending
library(plyr)
newstatesortdescending = arrange(newstate,desc(verbatim))
View(newstatesortdescending) #look at the output to see if there are trees in the first row
#added this code for IN because there were some corners from Morton's records that weren't entered in our MySQL database
#but came as a spreadsheet from Morton and the spreadsheet didn't have all the column headings (e.g., species, speciescode)
#as if the data had been entered into MySQL. So wanted to sort by L1_tree1 because that was brought in from the Morton data.
newstatesortdescending = arrange(newstate,desc(L1_tree1))
View(newstatesortdescending) #look at the output to see if there are trees in the first row


#make L1_tree1 lower case and without periods in order to compare masterunique trees below 
#this is for situations where 2 students enter the same tree but one student capitalizes and the other doesn't

newstate$L1_tree1 = tolower(newstate$L1_tree1)
newstate$L1_tree2 = tolower(newstate$L1_tree2)
newstate$L1_tree3 = tolower(newstate$L1_tree3)
newstate$L1_tree4 = tolower(newstate$L1_tree4)

newstate$L1_tree1 = gsub("[.]","",newstate$L1_tree1)
newstate$L1_tree2 = gsub("[.]","",newstate$L1_tree2)
newstate$L1_tree3 = gsub("[.]","",newstate$L1_tree3)
newstate$L1_tree4 = gsub("[.]","",newstate$L1_tree4)


#concatenate names,diameter and bearing info for tree1, tree2, tree3 and tree4 
uniquetree1 = paste0(newstate$L1_tree1,newstate$diameter,newstate$bearing,newstate$degrees,newstate$bearingdir,newstate$chainstree)
uniquetree2 = paste0(newstate$L1_tree2,newstate$diameter2,newstate$bearing2,newstate$degrees2,newstate$bearingdir2,newstate$chainstree2)
uniquetree3 = paste0(newstate$L1_tree3,newstate$diameter3,newstate$bearing3,newstate$degrees3,newstate$bearingdir3,newstate$chainstree3)
uniquetree4 = paste0(newstate$L1_tree4,newstate$diameter4,newstate$bearing4,newstate$degrees4,newstate$bearingdir4,newstate$chainstree4)

newstate$uniquetree1 = uniquetree1
newstate$uniquetree2 = uniquetree2
newstate$uniquetree3 = uniquetree3
newstate$uniquetree4 = uniquetree4




temporary = newstate[,c("entry_id","page","uniquetree1","uniquetree2","uniquetree3","uniquetree4")]
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

#remove entries with NANANA(IL) or NANA NA NA(IN) from tree 2 before you compare it to tree 1
temporary.2 <- temporary[!(temporary$uniquetree2 %in% c("NANANA")),]
temporary.2 <- temporary[!(temporary$uniquetree2 %in% c("NANA NA NA")),]
#TREE1 vs TREE2
#compare TREE1 vs TREE2 - these will have the greatest chance of having duplicates
tree1_vs_tree2 = 1:length(temporary.2$uniquetree1)
tree1_vs_tree2 = temporary.2$uniquetree1==temporary.2$uniquetree2
temporary.2$tree1_vs_tree2 = tree1_vs_tree2

#this finds the duplicates in trees1 and trees2 for each entry
temporary.2[which(temporary.2$tree1_vs_tree2 == "TRUE"),]
#save the output to make corrections in MySQL/Qualtrics/the state csv
write.csv(temporary.2, file = "C:/Users/jmurray7/Dropbox/GIS PalEON/IL_IN_WI Unprojected/Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/INtree1_v_tree2.csv", row.names = FALSE)

#remove entries with NANANA(IL) or NANA NA NA(IN) from tree 3
temporary.3 <- temporary.2[!(temporary.2$uniquetree3 %in% c("NANANA")),]
temporary.3 <- temporary.2[!(temporary.2$uniquetree3 %in% c("NANA NA NA")),]
#TREE3 vs TREE2
#look for duplicates for TREE3 compared to TREE2
tree2_vs_tree3 = 1:length(temporary.3$uniquetree2)
tree2_vs_tree3 = temporary.3$uniquetree2==temporary.3$uniquetree3
temporary.3$tree2_vs_tree3 = tree2_vs_tree3

#this finds the duplicates in TREE2 and TREE3 for each entry
temporary.3[which(temporary.3$tree2_vs_tree3 == "TRUE"),]
write.csv(temporary.3, file = "C:/Users/jmurray7/Dropbox/GIS PalEON/IL_IN_WI Unprojected/Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/INtree2_v_tree3.csv", row.names = FALSE)
#TREE3 vs TREE1
#look for duplicates for TREE3 compared to TREE1
tree1_vs_tree3 = 1:length(temporary.3$uniquetree1)
tree1_vs_tree3 = temporary.3$uniquetree1==temporary.3$uniquetree3
temporary.3$tree1_vs_tree3 = tree1_vs_tree3

#this finds the duplicates in TREE1 and TREE3 for each entry
temporary.3[which(temporary.3$tree1_vs_tree3 == "TRUE"),]


#remove entries with NANANA(IL) or NANA NA NA(IN) from tree 4
temporary.4 <- temporary.3[!(temporary.3$uniquetree4 %in% c("NANANA")),]
temporary.4 <- temporary.3[!(temporary.3$uniquetree4 %in% c("NANA NA NA")),]
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

#concatenate all uniquetree 1, 2, 3 and 4 information from the newstate into one vector
masteruniquetree = paste0(newstate$uniquetree1,newstate$uniquetree2,newstate$uniquetree3,newstate$uniquetree4)

newstate$masteruniquetree = masteruniquetree

uniquetemporary = newstate[,c("TRP","page","entry_id","reader_initials","hubtack_county","cornerid","masteruniquetree")]

#now check for duplicates in this list
uniquecount = unique(uniquetemporary$masteruniquetree)
length(uniquecount)
length(uniquecount) == length(uniquetemporary$masteruniquetree) #needs to say TRUE

# If the above says "FALSE" find the duplicated masteruniquetrees
uniquedups <-uniquetemporary[duplicated(uniquetemporary$masteruniquetree)|duplicated(uniquetemporary$masteruniquetree, fromLast=TRUE),]
uniquedups2 = uniquedups[order(uniquedups$masteruniquetree),] 
write.csv(uniquedups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/uniquetree_duplicates_v1.8-1_2.csv", row.names = FALSE)
write.csv(uniquedups2, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/uniquetree_duplicates_vIN1.7_take3.csv", row.names = FALSE)


#############################################
## Check that L1_treex + species match up ###
#############################################

#TREE1
library(plyr)
L1tree1.species <- ddply(newstate, .(newstate$species, newstate$L1_tree1), nrow)
L1tree1.species
write.csv(L1tree2.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree1-species.csv", row.names = FALSE)
write.csv(L1tree1.species, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree-species_take2.csv", row.names = FALSE)
#check L1_tree1 and species1 that do not match
newstate[which(newstate$L1_tree1 == "blue ash"),]


#TREE2
L1tree2.species <- ddply(newstate, .(newstate$species2, newstate$L1_tree2), nrow)
L1tree2.species
write.csv(L1tree2.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree2-species.csv", row.names = FALSE)
write.csv(L1tree2.species, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L2tree-species_take2.csv", row.names = FALSE)
#check L1_tree2 and species2 that do not match
newstate[which(newstate$L1_tree2 == "buckhorn"),]


#TREE3
L1tree3.species <- ddply(newstate, .(newstate$species3, newstate$L1_tree3), nrow)
L1tree3.species
write.csv(L1tree3.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree3-species.csv", row.names = FALSE)
write.csv(L1tree3.species, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L3tree-species.csv", row.names = FALSE)
#check L1_tree3 and species3 that do not match
newstate[which(newstate$L1_tree3 == "blue ash"),]


#TREE4
L1tree4.species <- ddply(newstate, .(newstate$species4, newstate$L1_tree4), nrow)
L1tree4.species
write.csv(L1tree4.species, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree4-species.csv", row.names = FALSE)
write.csv(L1tree4.species, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L4tree-species.csv", row.names = FALSE)
#check L1_tree4 and species4 that do not match
newstate[which(newstate$L1_tree4 == "blue ash"),]


##############################################
## Check that L1_treex + L3_tree2 match up ###
##############################################

#TREE1
library(plyr)
L1tree1.L3tree1 <- ddply(newstate, .(newstate$L3_tree1, newstate$L1_tree1), nrow)
L1tree1.L3tree1
write.csv(L1tree1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree1-L3tree1_2.csv", row.names = FALSE)
write.csv(L1tree1.L3tree1, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree1-L3tree1.csv", row.names = FALSE)
#check L1_tree1 and L3_tree1 that do not match
trees[which(trees$L1_tree1 == "blue ash"),]


#TREE2
L1tree2.L3tree2 <- ddply(newstate, .(newstate$L3_tree2, newstate$L1_tree2), nrow)
L1tree2.L3tree2
write.csv(L1tree2.L3tree2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree2-L3tree2_2.csv", row.names = FALSE)
write.csv(L1tree2.L3tree2, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree2-L3tree2.csv", row.names = FALSE)
#check L1_tree2 and L3_tree2 that do not match
trees[which(trees$L1_tree2 == "blue ash"),]


#TREE3
L1tree3.L3tree3 <- ddply(newstate, .(newstate$L3_tree3, newstate$L1_tree3), nrow)
L1tree3.L3tree3
write.csv(L1tree3.L3tree3, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree3-L3tree3_2.csv", row.names = FALSE)
write.csv(L1tree3.L3tree3, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree3-L3tree3.csv", row.names = FALSE)
#check L1_tree3 and L3_tree3 that do not match
trees[which(trees$L1_tree3 == "blue ash"),]


#TREE4
L1tree4.L3tree4 <- ddply(newstate, .(newstate$L3_tree4, newstate$L1_tree4), nrow)
L1tree4.L3tree4
write.csv(L1tree4.L3tree4, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree4-L3tree4_2.csv", row.names = FALSE)
write.csv(L1tree4.L3tree4, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree4-L3tree4.csv", row.names = FALSE)
#check L1_tree4 and L3_tree4 that do not match
trees[which(trees$L1_tree4 == "blue ash"),]

######
#combine all the L1 and L3 trees, to get a table with counts of all 4 trees L1/L3 labels
#select L1 & L3 of just tree1
L1.L3tree1 = newstate[,c("L1_tree1","L3_tree1")]
colnames(L1.L3tree1) = c("L1_tree", "L3_tree")

#select L1 & L3 of just tree2 and remove NAs
L1.L3tree2 = newstate[,c("L1_tree2","L3_tree2")]
L1.L3tree2 = L1.L3tree2[complete.cases(L1.L3tree2),]
colnames(L1.L3tree2) = c("L1_tree", "L3_tree")

#select L1 & L3 of just tree3 and remove NAs
L1.L3tree3 = newstate[,c("L1_tree3","L3_tree3")]
L1.L3tree3 = L1.L3tree3[complete.cases(L1.L3tree3),]
colnames(L1.L3tree3) = c("L1_tree", "L3_tree")

#select L1 & L3 of just tree4 and remove NAs
L1.L3tree4= newstate[,c("L1_tree4","L3_tree4")]
L1.L3tree4 = L1.L3tree4[complete.cases(L1.L3tree4),]
colnames(L1.L3tree4) = c("L1_tree", "L3_tree")

#combine all 4 L1&L3 trees
combined = rbind(L1.L3tree1,L1.L3tree2,L1.L3tree3,L1.L3tree4)

#create a table of the counts of L1 trees in the L3 categories
library(dplyr)
L1.L3combined = combined %>% group_by(L3_tree,L1_tree) %>% tally()

write.csv(L1.L3combined, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1-L3trees_summary.csv", row.names = FALSE)

######################################
### Check for Duplicate X,Y points ###
######################################


#Check for duplicates in the point x values
cornersPointXcount = unique(state$x)
length(cornersPointXcount)
length(cornersPointXcount) == length(state$x) #needs to say TRUE
#In IN this says FALSE, but there is one corner that has the same xs, but differnt ys, so is okay - see below
#for the specific corners that have the same xs, but different ys.

# If the above says "FALSE" find the duplicate state$x
xdups <-state[duplicated(state$x)|duplicated(state$x, fromLast=TRUE),]
xdups
xdups2 = xdups[order(xdups$x),]
xdups2 = xdups[order(xdups$entry_id),]#use this after you have corrected the duplicated xs. 
xdups2[,1:5] #you can compare the corners thave have duplicate xs, but different ys that are supposed to be like that.

#for IN v1.7 there are 12 corners (6 pairs) with the same x coordinates but different y coordinates

write.csv(xdups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/x_duplicates.csv", row.names = FALSE)
write.csv(xdups2, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/x_duplicates.csv", row.names = FALSE)


#Check for duplicates in the point y values
cornersPointYcount = unique(state$y)
length(cornersPointYcount)
length(cornersPointYcount) == length(state$y) #needs to say TRUE

# If the above says "FALSE" find the duplicate state$y
ydups <-state[duplicated(state$y)|duplicated(state$y, fromLast=TRUE),]
ydups
ydups2 = ydups[order(ydups$y),] 
ydups2[,1:29]
#for IN v1.7 there are 4 corners (2 pairs) with the same y coordinates but different x coordinates

write.csv(ydups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/y_duplicates.csv", row.names = FALSE)
write.csv(ydups2, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/y_duplicates.csv", row.names = FALSE)



#####################################################
### Check the Bearing/Bearingdir in all 4 trees   ###
#####################################################

#concatenate bearing and bearingdir
b1.check = paste0(newstate$bearing,newstate$bearingdir)
b2.check = paste0(newstate$bearing2,newstate$bearingdir2)
b3.check = paste0(newstate$bearing3,newstate$bearingdir3)
b4.check = paste0(newstate$bearing4,newstate$bearingdir4)

newstate$b1.check = b1.check
newstate$b2.check = b2.check
newstate$b3.check = b3.check
newstate$b4.check = b4.check

bearing.combo = as.data.frame(c("8888888888","8888899999","9999999999","9999988888","88888E","88888W",
                                "99999E","99999W","S88888","S99999","N88888","N99999","NANA","NE","NW",
                                "SE","SW","ENA","WNA","SNA","NNA","  "))
colnames(bearing.combo) = c("ComboName")


bearing1 = as.data.frame(table(unique(b1.check)))
setdiff(bearing1$Var1, bearing.combo$ComboName) #bearing1 values that are NOT in the bearing.combo list
setdiff(bearing.combo$ComboName, bearing1$Var1) #this is less important, but nice for a check
#it is the bearing.combo values that are NOT in the bearing1 list
#find the bearing/bearingdir combination that is not in the list
newstate[which(newstate$b1.check == "W99999"),]


bearing2 = as.data.frame(table(unique(b2.check)))
setdiff(bearing2$Var1, bearing.combo$ComboName) #bearing2 values that are NOT in the bearing.combo list
setdiff(bearing.combo$ComboName, bearing2$Var1) #this is less important, but nice for a check
#it is the bearing.combo values that are NOT in the bearing2 list
#find the bearing/bearingdir combination that is not in the list
newstate[which(newstate$b2.check == "W99999"),]
#NAE    W99999 WE


bearing3 = as.data.frame(table(unique(b3.check)))
setdiff(bearing3$Var1, bearing.combo$ComboName) #bearing3 values that are NOT in the bearing.combo list
setdiff(bearing.combo$ComboName, bearing3$Var1) #this is less important, but nice for a check
#it is the bearing.combo values that are NOT in the bearing3 list
#find the bearing/bearingdir combination that is not in the list
newstate[which(newstate$b3.check == "EN"),]


bearing4 = as.data.frame(table(unique(b4.check)))
setdiff(bearing4$Var1, bearing.combo$ComboName) #bearing4 values that are NOT in the bearing.combo list
setdiff(bearing.combo$ComboName, bearing4$Var1) #this is less important, but nice for a check
#it is the bearing.combo values that are NOT in the bearing4 list
#find the bearing/bearingdir combination that is not in the list
newstate[which(newstate$b4.check == "EN"),]




######################################
### Check for Duplicate entryIDs   ###
######################################

#In ILLINOIS there are corners which were not entered in MySQL because there was no data for the whole township or border
#and instead of entering each corner in MySQL, I entered them in the csv used for georeferencing only.
#all these entries have entry_id equal to 0 or NA.
#so need to remove all these entries first before comparing entry_ids
#In INDIANA there are corners that were entered by Sam Halsey at the Morton Arboretum. I converted that data
#to be in the same format as our data, however, there are no entry_ids for their data. So those corners have
#no entry_id values. They need to be removed before comparing entry_ids for Indiana

#remove entries with 0 and NA values in the entry_id
entryid.state <- state[!(state$entry_id %in% c(0,NA)),]
#just for fun select only entries with 0 and NA for the entry_ids and make sure that the total for
#nonentryid.state and entryid.state equals the total entries for state
nonentryid.state <- state[(state$entry_id %in% c(0,NA)),]
#nonentryid.IL = 249, entryid.IL = 57062, IL = 57311. They match!
#nonentryid.IN = 1459, entryid.IN = 55606, IN = 57065. They match!

unique.entryid = unique(entryid.state$entry_id)
length(unique.entryid)
length(unique.entryid) == length(entryid.state$entry_id) #needs to say TRUE


# If the above says "FALSE" find the duplicate entryid.state$entry_id
entryid.dups <-entryid.state[duplicated(entryid.state$entry_id)|duplicated(entryid.state$entry_id, fromLast=TRUE),]
entryid.dups2 = entryid.dups[order(entryid.dups$entry_id),] 
write.csv(entryid.dups2, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/entryid_duplicates.csv", row.names = FALSE)
write.csv(entryid.dups2, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/entryid_duplicates.csv", row.names = FALSE)

#the IN version 1.7 duplicate entryids had all been for Morton entries. Relabeled them as 0 for entry_ids


################################################################################
#### Check that there are entries for all State, Surveyor, Year, County  #######
################################################################################
#can use code below, but may be easier to sort in excel and find if there are any missing at the bottom

rm(list=ls())
state = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/ndilpls_v1.8-1.csv", header = TRUE, stringsAsFactors = FALSE)
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)


#STATE
state.state = state[order(state$state),] 
View(state.state)

#sort descending
library(plyr)
state_desc = arrange(state,desc(state))
View(state_desc) #look at the output to see if there are state in the first row

#create a table of the counts of state entries
library(dplyr)
statecount = state %>% group_by(state) %>% tally()
statecount

sum(statecount$n)

#SURVEYOR
surveyor = state[order(state$surveyor_name),] 
View(surveyor)

#sort descending
library(plyr)
surveyor_desc = arrange(state,desc(surveyor_name))
View(surveyor_desc) #look at the output to see if there are surveyors in the first row

#create a table of the counts of surveyor entries
library(dplyr)
surveyorcount = state %>% group_by(surveyor_name) %>% tally()
surveyorcount
View(surveyorcount)

sum(surveyorcount$n)

#YEAR
year = state[order(state$year),] 
View(year)

#sort descending
library(plyr)
year_desc = arrange(state,desc(year))
View(year_desc) #look at the output to see if there are surveyors in the first row

#create a table of the counts of year entries
library(dplyr)
yearcount = state %>% group_by(year) %>% tally()
yearcount
View(yearcount)

sum(yearcount$n)

#COUNTY
county = state[order(state$hubtack_county),] 
View(county)

#sort descending
county_desc = arrange(state,desc(hubtack_county))
View(county_desc) #look at the output to see if there are surveyors in the first row

#create a table of the counts of County entries
library(dplyr)
countycount = state %>% group_by(hubtack_county) %>% tally()
View(countycount)

sum(countycount$n)

#VERSION
version = state[order(state$version),] 
View(version)

#sort descending
version_desc = arrange(state,desc(version))
View(version_desc) #look at the output to see if there are surveyors in the first row

#create a table of the counts of version entries
library(dplyr)
versioncount = state %>% group_by(version) %>% tally()
versioncount

sum(versioncount$n)


################################################################
#### Check that Water/Wet entries have no trees entered ########
################################################################

watercheck <- state[(state$water %in% c("Water","Wet","water","wet")),]
nonwatercheck <- state[!(state$water %in% c("Water","Wet","water","wet")),]
#IL 56649+662 =57311. It matches!
#IN 1318 + 55747 = 57065. It matches!

write.csv(watercheck[order(watercheck$L3_tree1),], file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/water_check.csv", row.names = FALSE)
write.csv(watercheck[order(watercheck$L3_tree1),], file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/water_check.csv", row.names = FALSE)


################################################################
#### Summary Code to Use for ReadMe File                ########
################################################################
rm(list=ls())
state = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/ndilpls_v1.8-1.csv", header = TRUE, stringsAsFactors = FALSE)
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)


readers = as.data.frame(sort(unique(state$reader_initials)))

reader.table = as.data.frame(sort(table(state$reader_initials)))
reader.table
#compare the names in reader.table to the reader.table$names listed below.
#version IL 1.8 reader names
reader.table$ILnames = c("Sam Pecoraro", "Jill Deines", "Christina Wiech", "Will Tintor", "Jaclyn Cooney", "Rebecca O'Neil", 
          "Margaret Corcoran", "Benjamin Foster", "Grace Saalman", "Andrew Muench", "Nicole Fantozzi", "Nikki Micelotta",
          "Claire Mattison", "Isaac Evans", "Annie Han", "Will Chronister", "Kate Augustine", "Garrett Blad", 
          "Michelle Mueller", "Hannah Legatzke", "Zoe Volenec", "Kaitlin Powers","Anna Levesque", "Jody Peters", 
          "Emily Mears", "Caitlin Broderick", "Kim Bauer", "Amanda Buerger", "Alec Helmke", "Erin Nguyen","Da Som Kim", 
          "Mariel Cuellar", "Marissa Kivi", "Quercus Hamlin", "Bridget Bruns")

#version IN 1.7 reader names 
reader.table$INnames = c("Kelly Heilman", "Garrett Blad", "Hannah Legatzke", "Margaret Corcoran", "Benjamin Foster",
                         "Jaclyn Cooney", "Mairead Willis", "Jody Peters", "Zoe Volenec","Michelle Mueller", "Emily Mears",
                         "Will Chronister", "Nikki Micelotta", "Grace Saalman", "Kaitlin Powers", "Samniqueka Halsey, Morton Arboretum", 
                          "Emily Miller", "Annie Han", "Erin Nguyen", "Alec Helmke", "Anna Levesque", "Kim Bauer", "Bridget Bruns",
                         "Mariel Cuellar", "Marissa Kivi", "Amanda Buerger", "Da Som Kim", "Caitlin Broderick", "Quercus Hamlin")

reader.table

length(reader.table$Var1)
#35 students & Jody and Jill entered data for Illinois in version 1.8
#29 students & Jody entered data for Indiana in version 1.7

#number of trees 
#you can get the number of corners by the total number of observations in the file
#IN_v1.7 has 57065 corners total

#and you can get the number of corners with trees from the number of observations in the newstate object
newstate <- state[!(state$L3_tree1 %in% c("Water","Wet","No data", "No tree")),] #removes Water, Wet, No data, 
#and No tree entries so only entries with trees are included in the new dataframe
#IN_v1.7 has 49463 corners with trees

#now need to get the number of trees in that newstate object
tree1count = as.data.frame(table(newstate$L3_tree1))
t1 = sum(tree1count$Freq)
tree2count = as.data.frame(table(newstate$L3_tree2))
t2 = sum(tree2count$Freq)
tree3count = as.data.frame(table(newstate$L3_tree3))
t3 = sum(tree3count$Freq)
tree4count = as.data.frame(table(newstate$L3_tree4))
t4 = sum(tree4count$Freq)

tree.sum = c(t1,t2,t3,t4,sum(t1,t2,t3,t4))
tree.sum.names = c("tree1","tree2","tree3","tree4","total")
tree.sum.df = as.data.frame(tree.sum,tree.sum.names)
tree.sum.df
#IN_v1.7 has 97163 trees


#table of years sorted by year
year.table = as.data.frame(table(state$year))
year.table
#table of years sorted by frequency
year.table2 = as.data.frame(sort(table(state$year)))
year.table2

#plot of number of records surveyed in each year. MAKE SURE TO SAVE THIS PLOT IN THE RIGHT DIRECTORY
library(ggplot2)
year = ggplot(year.table, aes(Var1,Freq))+geom_bar(stat="identity")+theme_minimal()+ylab("Count")+theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
#png(paste("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/Count of Surveyor Year.png", sep = " "),   height = 768, width=1024)
png(paste("./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/Count of Surveyor Year.png", sep = " "),   height = 768, width=1024)
year
dev.off()


length(unique(state$TRP))
#Data came from 559 townships for Illinois in version 1.8
#Data came from 561 townships for Indiana in version 1.7

TRP.table = as.data.frame(table(state$TRP))
TRP = ggplot(TRP.table, aes(Var1,Freq))+geom_bar(stat="identity")+theme_minimal()+ylab("Count")+theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
TRP
hist(TRP.table$Freq, breaks = 100, xlim=c(1,115), xaxt='n',xlab = "Number of PLS Corners", main = "Histogram of TRP Corner \nMajority of Counts should be in the 107-110 range ",las=1)
axis(side=1, at=seq(0,115,5), labels=seq(0,115,5))

histinfo = hist(TRP.table$Freq, breaks = 100, xlim=range(1:115))
histinfo

#Histogram of the number of PLS corners for each township. MAKE SURE TO SAVE THE PLOT TO THE RIGHT DIRECTORY!
#png(paste("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/TRP Corner Counts.png", sep = " "),   height = 768, width=1024)
png(paste("./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/TRP Corner Counts.png", sep = " "),   height = 768, width=1024)
hist(TRP.table$Freq, breaks = 100, xlim=c(1,115), xaxt='n',xlab = "Number of PLS Corners", main = "Histogram of TRP Corner \nMajority of Counts should be in the 107-110 range ",las=1)
axis(side=1, at=seq(0,115,5), labels=seq(0,115,5))
dev.off()



#gives a summary of the number of corners to check out townships with min and max 
summary(TRP.table$Freq) 
#for IL version 1.8 shows 1 township has 3 corners and 1 township has 112 corners.  Find these townships
#for IN version 1.7 shows 1 township has min = 1. Find ths townships
TRP.table[which(TRP.table$Freq == 1),]
#for IL version 1.8 the township with 3 corners is 26N10W2 - this is a sliver township on the border of Indiana. It is fine.
#for IN version 1.7 the township with 1 corner is 38N10W2 - it is 1 section at the far northwest border of Indiana with Illinois. It is fine.
TRP.table[which(TRP.table$Freq == 112),]
#for IL version 1.8 the township with 112 corners is 17N11W2 - this is a strangely shapped township with an extra protrusion to the south which included
#extra corners.  It is fine.

#############################################
####### Version Number Counts ###############
#############################################

#IL is listed first, then IN below it

######################################
####### IL VERSION 1.8 ###############
######################################

#number of townships added for IL version 1.8
v1.8 = table(state$TRP,state$version)
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
#unique IL version values: 0, 1.6, 1.7, 1.8

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


######################################
####### IN VERSION 1.7 ###############
######################################

#number of townships added for IN version 1.7
v1.7 = table(state$TRP,state$version)
v1.7db = as.data.frame(v1.7)
View(v1.7db)
colnames(v1.7db) = c("TRP", "version","corners")
head(v1.7db)

sorted = v1.7db[order(v1.7db$version,v1.7db$corners),]
head(sorted)
tail(sorted)

sort2 <- subset(sorted, corners > 0, select=c(TRP,version,corners))
View(sort2)

unique(sort2$version) 
#unique IN version values: 1.5-1, 1.6, 1.7

version1.7 = subset(sort2,version == 1.7, select=c(TRP,version,corners))
View(version1.7[order(version1.7$TRP),])
version1.7$TRP
#144 IN version 1.7 townships. 5 of these townships had been originally entered in version 1.5-1
#(6N8W2, 12N9W2, 15N3W2, 18N4E2, 8N10W2) and three of these townships (9N9W2, 24N9E2, 8N4W2)
#had originally been entered in version 1.6, but had some additional corners added in version 1.7. 
#So the version 1.7 count is 136


version1.6 = subset(sort2,version == 1.6, select=c(TRP,version,corners))
View(version1.6)
version1.6$TRP
#161 version 1.6 townships. 

version1.5.1 = subset(sort2,version == "1.5-1", select=c(TRP,version,corners))
View(version1.5.1)
version1.5.1$TRP
#264 version 1.5-1 townships


##########################################################
#### Diameter, Degree, and Distance(Links) Checks ########
##########################################################
rm(list=ls())
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)

#DIAMETER COUNTS
#1. Run the code below for each tree diameter. 
#2. Then if there are any entries over 60 inches in diameters, use this to find them and double check that they have been double checked
library(dplyr)

#tree1 diameter
diameter1 = as.data.frame(table(state$diameter)) 
diameter1 #scroll through the diameters looking for small or large weird values.
state[which(state$diameter == 2.5),]
diameter.check = as.data.frame(state[which(state$diameter >= 60 & state$diameter < 88888),c(1,5,7,10,20,21,22,23,29,35,85,86,87,88)])
View(diameter.check)
write.csv(diameter.check[order(diameter.check$diameter),], file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/diameter1_check.csv", row.names = FALSE)


#tree2 diameter
diameter2 = as.data.frame(table(state$diameter2)) 
diameter2 #scroll through the diameters looking for small or large weird values.
state[which(state$diameter2 == 0),]
state[which(state$diameter2 == 1),]
diameter.check2 = as.data.frame(state[which(state$diameter2 >= 60 & state$diameter2 < 88888),c(1,5,7,10,20,21,22,23,29,45,85,86,87,88)])
View(diameter.check2)
write.csv(diameter.check2[order(diameter.check2$diameter2),], file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/diameter2_check.csv", row.names = FALSE)


#tree3 diameter
diameter3 = as.data.frame(table(state$diameter3)) #scroll through the diameters looking for small or large weird values.
diameter3 #scroll through the diameters looking for small or large weird values.
diameter.check3 = as.data.frame(state[which(state$diameter3 >= 60 & state$diameter3 < 88888),c(1,5,7,10,20,21,22,23,29,55,85,86,87,88)])
write.csv(diameter.check3[order(diameter.check3$diameter3),], file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/diameter3_check.csv", row.names = FALSE)

#tree4 diameter
diameter4 = as.data.frame(table(state$diameter4)) #scroll through the diameters looking for small or large weird values.
diameter4
diameter.check4 = as.data.frame(state[which(state$diameter4 >= 60 & state$diameter4 < 88888),c(1,5,7,10,20,21,22,23,29,65,85,86,87,88)])
write.csv(diameter.check4[order(diameter.check4$diameter4),], file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/diameter4_check.csv", row.names = FALSE)


####################
#degree counts

rm(list=ls())
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)

#if there are any entries over 90 degrees, use this to find them and double check that they have been double checked
state[which(state$degrees2 == "150"),]

#tree1 degree
degree1 = as.data.frame(table(state$degrees)) #scroll through the degrees looking for values over 90 or weird values.
degree1
tail(degree1)
state[which(state$degrees == 91),]

#tree2 degree
degree2 = as.data.frame(table(state$degrees2)) #scroll through the degrees looking for values over 90 or weird values.
degree2
tail(degree2)
state[which(state$degrees2 == 95),]
state[which(state$degrees2 == 99.5),]

#tree3 degree
degree3 = as.data.frame(table(state$degrees3)) #scroll through the degrees looking for values over 90 or weird values.
degree3

#tree4 degree
degree4 = as.data.frame(table(state$degrees4)) #scroll through the degrees looking for values over 90 or weird values.
degree4


####################
#chainstree(links) counts
rm(list=ls())
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)

#if there are any entries you want to check, use this to find them and double check that they have been double checked
state[which(state$chainstree == "22.85"),]

#tree1 chainstree
chainstree1 = as.data.frame(table(state$chainstree)) 
colnames(chainstree1) = c("chainstree", "count")
write.csv(chainstree1, "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/chainstree1.csv", row.names = FALSE)

#tree2 chainstree
chainstree2 = as.data.frame(table(state$chainstree2))
colnames(chainstree2) = c("chainstree2", "count")
write.csv(chainstree2, "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/chainstree2.csv", row.names = FALSE)

#tree3 chainstree
chainstree3 = as.data.frame(table(state$chainstree3))
colnames(chainstree3) = c("chainstree3", "count")
write.csv(chainstree3, "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/chainstree3.csv", row.names = FALSE)

#tree4 chainstree
chainstree4 = as.data.frame(table(state$chainstree4)) 
colnames(chainstree4) = c("chainstree4", "count")
write.csv(chainstree4, "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/chainstree4.csv", row.names = FALSE)




###########################################################
#### TRP check compared between GIS map and csv file ######
###########################################################

rm(list=ls())

#ILLINOIS
#find the TRPs that are not in ndilpls or not in GIS
#create the TRP_notduplicates.csv with a column for the GIS Township TRPs (you could also bring in the column with the Data Status label or the version number).
#then add a column for the TRPs from the ndilpls file - you can get this by doing a pivot table and getting the TRPs/count of TRP
TRP_notdups = read.csv("./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/TRP_notduplicates.csv", header = TRUE, stringsAsFactors = FALSE)
NotInGIS = TRP_notdups$ndilpls_TRP[!(TRP_notdups$ndilpls_TRP %in% TRP_notdups$GIS_TRP)]
NotInGIS
#This returned 40N1E3 and 39N1E3 which are in ndilpls but not in GIS. That is because the corners in this township are the west border corners and
#are actually the east border of 22N11E4 in Ogle county.  So this is okay.
#18N3E3 was also returned as being in ndilpls and not on the GIS map.  This had been marked on the map as 18N3E4. Corrected on GIS map.

NotInndilpls = TRP_notdups$GIS_TRP[!(TRP_notdups$GIS_TRP %in% TRP_notdups$ndilpls_TRP)]
NotInndilpls


#INDIANA
#create the TRP_notduplicates.csv with a column for the GIS Township TRPs (you could also bring in the column with the Data Status label or the version number).
#then add a column for the TRPs from the ndinpls file - you can get this by doing a pivot table and getting the TRPs/count of TRP
TRP_notdups = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/TRP_notduplicates.csv", header = TRUE, stringsAsFactors = FALSE)
NotInGIS = TRP_notdups$ndinplsTRP[!(TRP_notdups$ndinplsTRP %in% TRP_notdups$GISTRP)]
NotInGIS
#This returned 38N5W2. This had been a Morton township that was entered. Updated the GIS map.

NotIn_ndinpls = TRP_notdups$GISTRP[!(TRP_notdups$GISTRP %in% TRP_notdups$ndinplsTRP)]
NotIn_ndinpls
#5N10E2, 6N13E2, 8N1W1, and 9N1W1 are in the townships marked in GIS, but not in the ndinpls file
# 5N10E2, 8N1W1 and 9N1W1 are all marked as Illegible or Missing data in GIS. But there is no description 
#given for them in the Township Assignment Sheet. At some point Jody will go back and double check if she can find 
#the notes for these. If she can't and or if they are too illegible to enter, then Jody will add those township/notes
#to the Township Assignment sheet and enter these townships as No data in the Qualtrics database. For now she is leaving
#them out of the ndinpls_v1.7 records.
#Township 6N13E2 is a super small sliver. We have notes for this township, but it is so small that there are no 1/4 section
#or section corners. So it is marked on the GIS map, because we have the notes, but it is not in the database, because
#there are no corners for this township to enter.


#########################################################
## Get L1_treex & L3_treex table for Conversion Table ###
#########################################################

#this was done above when the L1 & L3 trees were checked. But can use the code again here.
rm(list=ls())
state = read.csv("./Indiana/IN PLS for Modelers/IN PLS_v1.7/ndinpls_v1.7_inprogress.csv", header = TRUE, stringsAsFactors = FALSE)
newstate <- state[!(state$L3_tree1 %in% c("Water","Wet","No data", "No tree")),] #removes Water, Wet, No data, 
#and No tree entries so only entries with trees are included in the new dataframe

#combine all the L1 and L3 trees, to get a table with counts of all 4 trees L1/L3 labels
#select L1 & L3 of just tree1
L1.L3tree1 = newstate[,c("L1_tree1","L3_tree1")]
colnames(L1.L3tree1) = c("L1_tree", "L3_tree")
write.csv(L1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree1-L3tree1.csv", row.names = FALSE)
write.csv(L1.L3tree1, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree1-L3tree1.csv", row.names = FALSE)
#check L1_tree1 and L3_tree1 that do not match
newstate[which(trees$L1_tree1 == "blue ash"),]


#select L1 & L3 of just tree2 and remove NAs
L1.L3tree2 = newstate[,c("L1_tree2","L3_tree2")]
L1.L3tree2 = L1.L3tree2[complete.cases(L1.L3tree2),]
colnames(L1.L3tree2) = c("L1_tree", "L3_tree")
write.csv(L1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree2-L3tree2.csv", row.names = FALSE)
write.csv(L1.L3tree1, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree2-L3tree2.csv", row.names = FALSE)


#select L1 & L3 of just tree3 and remove NAs
L1.L3tree3 = newstate[,c("L1_tree3","L3_tree3")]
L1.L3tree3 = L1.L3tree3[complete.cases(L1.L3tree3),]
colnames(L1.L3tree3) = c("L1_tree", "L3_tree")
write.csv(L1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree3-L3tree3.csv", row.names = FALSE)
write.csv(L1.L3tree1, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree3-L3tree3.csv", row.names = FALSE)


#select L1 & L3 of just tree4 and remove NAs
L1.L3tree4= newstate[,c("L1_tree4","L3_tree4")]
L1.L3tree4 = L1.L3tree4[complete.cases(L1.L3tree4),]
colnames(L1.L3tree4) = c("L1_tree", "L3_tree")
write.csv(L1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1_L1tree4-L3tree4.csv", row.names = FALSE)
write.csv(L1.L3tree1, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1tree4-L3tree4.csv", row.names = FALSE)

#combine all 4 L1&L3 trees
combined = rbind(L1.L3tree1,L1.L3tree2,L1.L3tree3,L1.L3tree4)

#create a table of the counts of L1 trees in the L3 categories
library(dplyr)
L1.L3combined = combined %>% group_by(L3_tree,L1_tree) %>% tally()

write.csv(L1.L3tree1, file = "./Illinois/IL PLS for Modelers/Illinois PLS_v1.8_6-6-17/QA_QC Output/IL1.8-1__L1-L3trees_summary.csv", row.names = FALSE)
write.csv(L1.L3combined, file = "./Indiana/IN PLS for Modelers/IN PLS_v1.7/QA_QC Output/IN1.7_L1-L3trees_summary2.csv", row.names = FALSE)

