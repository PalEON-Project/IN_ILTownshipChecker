# IN_ILTownshipChecker

There are two R code files used by ND to check and georeference the Indiana and Illinois PLS data.

I. The IN_ILTownshipChecker is the first check of Indiana and Illinois PLS data entered into ND's MySQL database.  For each township entered, there is another individual that will use the R check look for errors including:

1. typos and missing values
2. check valid section combinations
3. check valid section and section cornerID combinations
4. check valid quarter section and quarter section cornerID combinations
5. check that all interiorsection entries are valid section number for the borders
6. check that every bearing has a degrees value (and vice versa) and that bearing entries are N/S only and degrees are less than 90
7. check that every bearingdir corresponds to a chainstree value (and vice versa) and bearingdir are alwasy E/W
8. check chainstree for decimal values other than 0.0, 0.25, 0.50 and 0.75 
9. check for invalid values in refcorner or NAs
10. check for duplicate entries via refcorner, chains, traveldir and typecorner
11. check that the type of corner (section, quartersection, township) matches the cornerID
12. create list of entries that data enterers request to have rechecked by someone else
13. check for errors in the diameter, degrees, and verbatim trees and make sure there is a good description for why 88888s or 99999s were entered

II. Once the initial check is complete, the data entered in MySQL then is exported and georeferenced and checked again using the Georef code.  The data processing steps are:
Data Processing  Steps for ndinpls_v1.5-1, 1.6, and 1.7
1.	Added GIS pointX and pointY using the entryID and UniqueID for the corners.
2.	Left in Understory notes.
3.	Added column for the TRP (township, range, primemeridian concatenated)
4.	Added column for the version number
5.	We had added the waterwet column after the v1.5 data was entered, so went back and checked all No tree/No data in each township grouping to entries to make sure they were labeled correctly and labeled if they were Water or Wet.  
6.	Removed preceding spaces. Make new column (i.e., verbatim_b, verbatim2_b, etc). Sort by verbatim, then use formula =trim(cell#). Don’t use this formula for the 88888s and 99999s. Just copy those over.  Copy full column, then paste special. Then delete old verbatim column and rename verbatim_b as verbatim. 
7.	Sort by verbatim, then copy verbatim to L1_tree1 column, then sort by verbatim2 (and 3 and 4) then create L1_tree2 (and 3 and 4) by doing an if statement that changes “dittos”s (“, Do, etc) to the tree in the L1_tree1 column for that corner.  
8.	L3_tree1, tree2, tree3 and tree4: convert Trees 1, 2, 3, and 4 into Level 3 tree names 
9.	Put No Tree and No Data and Water in the L1_tree1 column (But not in the other 3 tree columns. Put in NAs in for the other 3 columns in step 12). 
10.	Put in NAs in L1_trees, L3_trees, diameter, degrees, chainstrees for Tree 2, 3, 4 with No Trees and for Tree 1,2,3,4 with No Trees, No Data, and Water. 
11.	Remove “Null” entries which will mainly be found in ecotype notes through the fixed column
13.	Create Pivot Table in new worksheet with Summary of 
a.	TRP counts 
b.	L3 then L1 trees (nested) with counts of L1 trees for all 4 trees (to make sure the L1 trees are in the correct L3 tree categories) (make sure all L1 trees are in the correct L3 category)
c.	Diameter counts for all 4 trees (make sure there aren’t any weird diameters – check diameters 60 or greater)
d.	Degrees counts for all 4 trees (make sure no degrees >90 or if so include a note) 
e.	Chainstrees counts for all 4 trees (look for any weird tree distances) 
f.	Check that all the pivot table counts are the same for diameter, degrees, chainstrees 
g.	Get counts of x and y coordinates. Make sure there is the same number of unique coordinates as there are TRPs.  Then make sure the counts for the TRPs, x and y coordinates match up. 
