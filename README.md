# IN_ILTownshipChecker

This is R code used by ND as a first check of Indiana and Illinois PLS data entered into our database.  For each township entered, there is another individual that will use the R check look for errors including:

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
