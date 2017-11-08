##Check all Townships for missing CornerIDs##
#Use for Indiana version 1.3 and Illinois version 1.2 Quality Checks##

## Corner completeness check ##

# add test values
# town [1,24] = "Intersection" # add test factors
# town [2,24] = "Temporary"  	# add test factors

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
