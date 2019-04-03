#####################################
#### Qualtrics Clean-Up Code ########
#####################################

#12-5-17 Jody Peters created this code to clean up the output that comes from Qualtrics
# this code lets students select just their individual townships

rm(list=ls())

##  SET WORKING DIRECTORY to tell R where to look for the csv files.  Only run the line below
#  that applies to the computer you are working on (or add your own).  The file path should
#  lead to the "Qualtrics Checks By Reader" --> "Original Qualtrics Download" folder on your computer.
setwd("C:/Users/paleolab/Dropbox/PalEON Lab Team/Qualtrics Checks By Reader") # Back Bench Computers
setwd("C:/Users/paleolab/Dropbox/PalEON Lab Team/Qualtrics Checks By Reader") # Middle Bench Desktop
setwd("C:/Users/paleolab/Dropbox/PalEON Lab Team/Qualtrics Checks By Reader") # Middle Bench Laptop
setwd("C:/Dropbox/PalEON Lab Team/Qualtrics Checks By Reader") # Back Bench Computer
setwd("C:/Users/jmurray7/Dropbox/PalEON Lab Team/Qualtrics Checks By Reader") # Jody's work computer
setwd("~/Dropbox/PalEON Lab Team/Qualtrics Checks By Reader/") # Back Bench Mac
# Back Bench Middle Computer




#load in your data. 
#Make sure to CHANGE THE NAME OF THE FILE TO MATCH WHAT YOU EXPORTED FROM QUALTRICS!!
mydata = read.csv(file = "Original Qualtrics Download/GLO_v20_Peters.csv",skip=1,header = TRUE, stringsAsFactors = FALSE)
#View(mydata)

#ENTER YOUR TOWNSHIP TRP, COUNTY AND STATE
township.name <- "28N1E4"

#Select the data columns
TRP = mydata$TRP
version =	mydata$Survey_Version
entry_id =	mydata$ResponseID.1
timestamp	=	mydata$EndDate
reader_initials = paste0(mydata$FName," ",mydata$LName)
surveyor_name	= mydata$Surveyor_Name
volume = mydata$Volume
page = mydata$Page
year	= mydata$Survey_Year
state	= mydata$Survey_State
hubtack_county	= mydata$Hubtack_County
baseline	= mydata$Baseline
meridian	= mydata$Meridian
township	= mydata$Township
townshipdir	= mydata$North_South
rangenum	= mydata$Survey_Range
rangedir	= mydata$East_West
sectiona	= mydata$Section.A
sectionb	= mydata$Section.B
interiorsection	= mydata$Number.of.Interior.Section
interiordir	= mydata$South.or.East.Border.
refcorner	= mydata$Reference.Corner
traveldir	= mydata$Direction.of.Travel..North..South..East..West.
secondpass	= mydata$Second.Pass
chains	= mydata$Chains..XX.XX.
typecorner	= mydata$Type.of.Corner
typecorner = gsub("1/4 Section","(1/4) Section",typecorner) #change 1/4 Section in type corner to (1/4) Section to be consistent with the rest of the PLS database
typecorner = gsub("1/4 section","(1/4) Section",typecorner) #change 1/4 section in type corner to (1/4) Section to be consistent with the rest of the PLS database
cornerid	= mydata$Corner.ID
species	= mydata$Species..1.
verbatim	= mydata$Verbatim..1.
diameter	= mydata$Diameter..in....1.
bearing	= mydata$Bearing..1.
degrees		= mydata$Degrees..1.
bearingdir	= mydata$Bearing.Direction..1.
chainstree	= mydata$Chainstrees..links...1.
species2	= mydata$Species..2.
verbatim2	= mydata$Verbatim..2.
diameter2	= mydata$Diameter..in....2.
bearing2	= mydata$Bearing..2.
degrees2	= mydata$Degrees..2.
bearingdir2	= mydata$Bearing.Direction..2.
chainstree2	= mydata$Chainstrees..links...2.
species3	= mydata$Species..3.
verbatim3	= mydata$Verbatim..3.
diameter3	= mydata$Diameter..in....3.
bearing3	= mydata$Bearing..3.
degrees3	= mydata$Degrees..3.
bearingdir3	= mydata$Bearing.Direction..3.
chainstree3	= mydata$Chainstrees..links...3.
species4	= mydata$Species..4.
verbatim4	= mydata$Verbatim..4.
diameter4	= mydata$Diameter..in....4.
bearing4	= mydata$Bearing..4.
degrees4	= mydata$Degrees..4.
bearingdir4	= mydata$Bearing.Direction..4.
chainstree4	= mydata$Chainstrees..links...4.
ecotype	= mydata$Ecotypes
ecotypenotes	= mydata$Ecotype.Notes
ecotype2	= mydata$Ecotypes2
ecotypenotes2	= mydata$Ecotype.Notes2
ecotype3	= mydata$Ecotypes3
ecotypenotes3	= mydata$Ecotype.Notes3
water = rep(NA,length(mydata$Water..wet..or.No.tree.))
for(i in 1:length(water)){
  if(mydata$Water..wet..or.No.tree.[i] == 'Water'){
    water[i] <- 'Water'
  }
  else if(mydata$Water..wet..or.No.tree.[i] == 'Wet'){
    water[i] <- 'Wet'
  }
}

nodata	= mydata$No.data.
nodata = gsub("NA","",nodata)
notree = rep(NA,length(mydata$Trees.))
for(i in 1:length(notree)){
  if(mydata$Trees.[i] == 'No'){
    notree[i] <- 'yes'
  }
}

feature	= mydata$Feature
featurenotes	= mydata$Feature.Notes..if.needed.
timbernotes	= rep(NA,length(mydata[,1])) #need to add a column to the database for this. It is extra because we no longer use it, but want to keep it in so all the databases are consistent when we merge them
understorynotes =	rep(NA,length(mydata[,1])) #need to add a column to the database for this. It is extra because we no longer use it, but want to keep it in so all the databases are consistent when we merge them
landnotes = paste0(mydata$Land.Notes.1st.Rate,mydata$Land.Notes.2nd.Rate,mydata$Land.Notes.3rd.Rate,mydata$Land.Notes.Barrens,
                   mydata$Land.Notes.Blowdown,mydata$Land.Notes.Boggy,mydata$Land.Notes.Broken.Land,mydata$Land.Notes.Burn,
                   mydata$Land.Notes.Dry,mydata$Land.Notes.Fire,mydata$Land.Notes.Fit.for.Cultivation,mydata$Land.Notes.Good.Soil,
                   mydata$Land.Notes.High,mydata$Land.Notes.Hilly,mydata$Land.Notes.Illegible,mydata$Land.Notes.Insect.Death,
                   mydata$Land.Notes.Level,mydata$Land.Notes.Low,mydata$Land.Notes.Marshy,mydata$Land.Notes.Other,
                   mydata$Land.Notes.Poor.Soil,mydata$Land.Notes.Prairie,mydata$Land.Notes.Rich,mydata$Land.Notes.Rolling,
                   mydata$Land.Notes.Sandy,mydata$Land.Notes.Swamp,mydata$Land.Notes.Thin.Soil,mydata$Land.Notes.Too.Wet,
                   mydata$Land.Notes.Unfit.for.Cultivation,mydata$Land.Notes.Upland,mydata$Land.Notes.Wet,mydata$Land.Notes.Woodland)	
landnotes = gsub("NA","",landnotes)

landnotesvb	= mydata$Land.Notes.Verbatim..if.needed.
generalnotes	= mydata$General.Notes
recheck	= mydata$Unsure.about.accuracy.of.entry.
reason	= mydata$Reason.for.re.examining.the.entry
fixed = rep(NA,length(mydata[,1]))

#Convert the species to speciescode
spcode = read.csv(file = "SpeciesCode_Conversion.csv", header = TRUE, stringsAsFactors = FALSE)
speciescode = rep(NA,length(mydata$Species..1.))
for(i in 1:length(speciescode)){
  speciescode[i] = spcode[match(mydata$Species..1.[i], spcode$Species, nomatch = NA),2]
}

speciescode2 = rep(NA,length(mydata$Species..2.))
for(i in 1:length(speciescode2)){
  speciescode2[i] = spcode[match(mydata$Species..2.[i], spcode$Species, nomatch = NA),2]
}

speciescode3 = rep(NA,length(mydata$Species..3.))
for(i in 1:length(speciescode3)){
  speciescode3[i] = spcode[match(mydata$Species..3.[i], spcode$Species, nomatch = NA),2]
}

speciescode4 = rep(NA,length(mydata$Species..4.))
for(i in 1:length(speciescode4)){
  speciescode4[i] = spcode[match(mydata$Species..4.[i], spcode$Species, nomatch = NA),2]
}

#combine all the data into a data frame
mydata_combined = as.data.frame(cbind(TRP,version,entry_id,timestamp,reader_initials,surveyor_name,volume,page,year,state,
                                      hubtack_county,baseline,meridian,township,townshipdir,rangenum,rangedir,sectiona,
                                      sectionb,interiorsection,interiordir,refcorner,traveldir,secondpass,chains,typecorner,
                                      cornerid,nodata,notree,species,speciescode,verbatim,diameter,bearing,degrees,bearingdir,chainstree,species2,
                                      speciescode2,verbatim2,diameter2,bearing2,degrees2,bearingdir2,chainstree2,species3,speciescode3,
                                      verbatim3,diameter3,bearing3,degrees3,bearingdir3,chainstree3,species4,speciescode4,verbatim4,
                                      diameter4,bearing4,degrees4,bearingdir4,chainstree4,ecotype,ecotypenotes,ecotype2,ecotypenotes2,
                                      ecotype3,ecotypenotes3,water,feature,featurenotes,timbernotes,understorynotes,
                                      landnotes,landnotesvb,generalnotes,recheck,reason,fixed))

#select just your township
#township_data = mydata_combined[which(mydata_combined$TRP == "11N11E2"),]
township_data = mydata_combined[which(mydata_combined$TRP == township.name),]

# TEST TO SEE IF THERE ARE ANY MISSING CORNERIDS
# Create vectors of the Internal Corners and the South and East borders
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
internalcorners.test = internal.corners[!(internal.corners %in% township_data$cornerid)]
SEbordercorners.test = SEborder.corners[!(SEborder.corners %in% township_data$cornerid)]

error = "cornerid not found in township data"

missing.corner = cbind(internalcorners.test,error)
missing.corner

missing.border = cbind(SEbordercorners.test,error)
missing.border

#check for any duplicated cornerIDs
cornerid.dups = township_data[duplicated(township_data$cornerid)|duplicated(township_data$cornerid, fromLast = TRUE),c("sectiona","sectionb","interiorsection","interiordir","traveldir","secondpass","chains","typecorner","cornerid","entry_id")]
cornerid.dups = cornerid.dups[order(cornerid.dups$cornerid),] 
cornerid.dups

township_data = township_data[order(township_data$cornerid),] 
#create a csv with data just from the township you selected above. MAKE SURE TO PUT IN THE RIGHT TRP NAME!
write.csv(township_data, paste0("./Checked Townships/", township.name, "_QualtricsCleaned.csv"), row.names = FALSE, na="")
 








#########################################################################################################
#EXTRA CODE

#removes the top two lines in of the dataframe which had the column names listed twice two different ways
mydata = mydata[-c(1,2),]

#select the data associated with each variable and give it a name
TRP = mydata[,35]
version =	mydata[,34]
entry_id =	mydata[,1]
timestamp	=	mydata[,9]
reader_initials = paste0(mydata[,36]," ",mydata[,37])
surveyor_name	= mydata[,22]
volume = mydata[,23]
page = mydata[,46]	
year	= mydata[,24]
state	= mydata[,25]
hubtack_county	= mydata[,26]
baseline	= mydata[,27]
meridian	= mydata[,28]
township	= mydata[,29]
townshipdir	= mydata[,30]
rangenum	= mydata[,31]
rangedir	= mydata[,32]
sectiona	= mydata[,44]
sectionb	= mydata[,45]
interiorsection	= mydata[,42]
interiordir	= mydata[,43]
refcorner	= mydata[,47]
traveldir	= mydata[,48]
secondpass	= mydata[,49]
chains	= mydata[,50]
typecorner	= mydata[,51]
cornerid	= mydata[,40]
species	= mydata[,57]
verbatim	= mydata[,61]
diameter	= mydata[,65]
bearing	= mydata[,73]
degrees		= mydata[,77]
bearingdir	= mydata[,85]
chainstree	= mydata[,89]
species2	= mydata[,58]
verbatim2	= mydata[,62]
diameter2	= mydata[,66]
bearing2	= mydata[,74]
degrees2	= mydata[,78]
bearingdir2	= mydata[,86]
chainstree2	= mydata[,90]
species3	= mydata[,59]
verbatim3	= mydata[,63]
diameter3	= mydata[,67]
bearing3	= mydata[,75]
degrees3	= mydata[,79]
bearingdir3	= mydata[,87]
chainstree3	= mydata[,91]
species4	= mydata[,60]
verbatim4	= mydata[,64]
diameter4	= mydata[,68]
bearing4	= mydata[,76]
degrees4	= mydata[,80]
bearingdir4	= mydata[,88]
chainstree4	= mydata[,92]
ecotype	= mydata[,102]
ecotypenotes	= mydata[,103]
ecotype2	= mydata[,104]
ecotypenotes2	= mydata[,105]
ecotype3	= mydata[,106]
ecotypenotes3	= mydata[,107]
water = rep(NA,length(mydata[,101]))
for(i in 1:length(water)){
  if(mydata[i,101] == 'Water'){
    water[i] <- 'Water'
  }
  else if(mydata[i,101] == 'Wet'){
    water[i] <- 'Wet'
  }
}

nodata	= mydata[,52]
notree = rep(NA,length(mydata[,53]))
for(i in 1:length(notree)){
  if(mydata[i,53] == 'No'){
    notree[i] <- 'yes'
  }
}

feature	= mydata[,108]
featurenotes	= mydata[,109]
timbernotes	= rep(NA,length(mydata[,1])) #need to add a column to the database for this. It is extra because we no longer use it, but want to keep it in so all the databases are consistent when we merge them
understorynotes =	rep(NA,length(mydata[,1])) #need to add a column to the database for this. It is extra because we no longer use it, but want to keep it in so all the databases are consistent when we merge them
landnotes = paste0(mydata[,110],mydata[,111],mydata[,112],mydata[,113],mydata[,114],mydata[,115],mydata[,116],mydata[,117],
                   mydata[,118],mydata[,119],mydata[,120],mydata[,121],mydata[,122],mydata[,123],mydata[,124],mydata[,125],
                   mydata[,126],mydata[,127],mydata[,128],mydata[,129],mydata[,130],mydata[,131],mydata[,132],mydata[,133],
                   mydata[,134],mydata[,135],mydata[,136],mydata[,137],mydata[,138],mydata[,139],mydata[,140],mydata[,141])	
landnotesvb	= mydata[,142]
generalnotes	= mydata[,54]
recheck	= mydata[,55]
reason	= mydata[,56]
fixed = rep(NA,length(mydata[,1]))


#Convert the species to speciescode
spcode = read.csv(file = "SpeciesCode_Conversion.csv", header = TRUE, stringsAsFactors = FALSE)
speciescode = rep(NA,length(mydata[,57]))
for(i in 1:length(speciescode)){
  speciescode[i] = spcode[match(mydata[i,57], spcode$Species, nomatch = NA),2]
}

speciescode2 = rep(NA,length(mydata[,58]))
for(i in 1:length(speciescode2)){
  speciescode2[i] = spcode[match(mydata[i,58], spcode$Species, nomatch = NA),2]
}

speciescode3 = rep(NA,length(mydata[,59]))
for(i in 1:length(speciescode3)){
  speciescode3[i] = spcode[match(mydata[i,59], spcode$Species, nomatch = NA),2]
}

speciescode4 = rep(NA,length(mydata[,60]))
for(i in 1:length(speciescode4)){
  speciescode4[i] = spcode[match(mydata[i,60], spcode$Species, nomatch = NA),2]
}

#combine all the data into a data frame
mydata_combined = as.data.frame(cbind(TRP,version,entry_id,timestamp,reader_initials,surveyor_name,volume,page,year,state,
                                      hubtack_county,baseline,meridian,township,townshipdir,rangenum,rangedir,sectiona,
                                      sectionb,interiorsection,interiordir,refcorner,traveldir,secondpass,chains,typecorner,
                                      cornerid,nodata,notree,species,speciescode,verbatim,diameter,bearing,degrees,bearingdir,chainstree,species2,
                                      speciescode2,verbatim2,diameter2,bearing2,degrees2,bearingdir2,chainstree2,species3,speciescode3,
                                      verbatim3,diameter3,bearing3,degrees3,bearingdir3,chainstree3,species4,speciescode4,verbatim4,
                                      diameter4,bearing4,degrees4,bearingdir4,chainstree4,ecotype,ecotypenotes,ecotype2,ecotypenotes2,
                                      ecotype3,ecotypenotes3,water,feature,featurenotes,timbernotes,understorynotes,
                                      landnotes,landnotesvb,generalnotes,recheck,reason,fixed))

#here is the option to export all the data from Qualtrics - probably won't need to use this very often
#write.csv(mydata_combined, "./Checked Townships/QualtricsClean.csv", row.names = FALSE, na="")

unique(mydata_combined$TRP) #this shows the different TRPs in the database

#select just the township that you want to check
township_data = mydata_combined[which(mydata_combined$TRP == "6S6E0Q"),]

#create a csv with data just from the township you selected above. MAKE SURE TO PUT IN THE RIGHT TRP NAME!
write.csv(township_data, "./Checked Townships/66E0Q_QualtricsCleaned.csv", row.names = FALSE, na="")






#select just the data that one student has entered
#student_data = mydata_combined[which(mydata_combined$reader_initials == "Emily Miller"),]
#create a csv with just the data from the person selected above. MAKE SURE TO PUT IN THE RIGHT TRP NAME!
#write.csv(township_data, "./Checked Townships/27N3E4_QualtricsCleaned.csv", row.names = FALSE, na="")

