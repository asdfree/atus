if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
atus_cat <- get_catalog( "atus" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( atus_cat ) ) / ceiling( nrow( atus_cat ) / 5 ) )
atus_cat <- atus_cat[ record_categories == this_sample_break , ]
lodown( "atus" , atus_cat )
if( any( atus_cat$directory == 2015 ) ){
library(lodown)
# examine all available ATUS microdata files
atus_cat <-
	get_catalog( "atus" ,
		output_dir = file.path( getwd() ) )

# 2015 only
atus_cat <- subset( atus_cat , directory == 2015 )
# download the microdata to your local computer


options( survey.replicates.mse = TRUE )
library(survey)

atusact <- readRDS( file.path( getwd() , "2015/atusact.rds" ) )
atusact <- atusact[ c( 'tucaseid' , 'tutier1code' , 'tutier2code' , 'tuactdur24' ) ]

atusresp <- readRDS( file.path( getwd() , "2015/atusresp.rds" ) )
atusresp <- atusresp[ c( 'tucaseid' , 'tufinlwgt' , 'tulineno' ) ]

atusrost <- readRDS( file.path( getwd() , "2015/atusrost.rds" ) )
atusrost <- atusrost[ , c( 'tucaseid' , 'tulineno' , 'teage' , 'tesex' ) ]

atuswgts <- readRDS( file.path( getwd() , "2015/atuswgts.rds" ) )
atuswgts <- atuswgts[ , c( 1 , grep( 'finlwgt' , names( atuswgts ) ) ) ]

# looking at the 2012 lexicon, travel-related activities
# have a tier 1 code of 18 --
# http://www.bls.gov/tus/lexiconnoex2012.pdf#page=22

# for all records where the tier 1 code is 18 (travel)
# replace that tier 1 of 18 with whatever's stored in tier 2
atusact[ atusact$tutier1code == 18 , 'tutier1code' ] <- atusact[ atusact$tutier1code == 18 , 'tutier2code' ]
# this will distribute all travel-related activities
# to the appropriate tier 1 category, which matches
# the structure of the 2012 bls table available at
# http://www.bls.gov/tus/tables/a1_2012.pdf

# sum up activity duration at the respondent-level
# *and* also the tier 1 code level
# (using tucaseid as the unique identifier)
# from the activities file
x <- aggregate( tuactdur24 ~ tucaseid + tutier1code , data = atusact , sum )

# now table `x` contains
# one record per person per major activity category

# reshape this data from "long" to "wide" format,
# creating a one-record-per-person table
y <- reshape( x , idvar = 'tucaseid' , timevar = 'tutier1code' , direction = 'wide' )

y[ is.na( y ) ] <- 0
# convert all missings to zeroes,
# since those individuals simply did not
# engage in those activities during their interview day
# (meaning they should have zero minutes of time)

# except for the first column (the unique identifier,
# replace each column by the quotient of itself and sixty
y[ , -1 ] <- y[ , -1 ] / 60
# now you've got an activity file `y`
# with one record per respondent

# merge together the data.frame objects with all needed columns
# in order to create a replicate-weighted survey design object

# merge the respondent file with the newly-created activity file
# (which, remember, is also one-record-per-respondent)
resp_y <- merge( atusresp , y )

# confirm that the result of the merge has the same number of records
# as the original bls atus respondent file. (this is a worthwhile check)
stopifnot( nrow( resp_y ) == nrow( atusresp ) )

# merge that result with the roster file
# note that the roster file has multiple records per `tucaseid`
# but only the `tulineno` columns equal to 1 will match
# records in the original respondent file, this merge works.
resp_y_rost <- merge( resp_y , atusrost )

# confirm that the result of the merge has the same number of records
stopifnot( nrow( resp_y_rost ) == nrow( atusresp ) )

# merge that result with the replicate weights file
z <- merge( resp_y_rost , atuswgts )

# confirm that the result of the merge has the same number of records
stopifnot( nrow( z ) == nrow( atusresp ) )

# remove dots from column names
names( z ) <- gsub( "\\." , "_" , names( z ) )

# add a column of ones
z$one <- 1

atus_design <- 
	svrepdesign(
		weights = ~tufinlwgt ,
		repweights = "finlwgt[1-9]" , 
		type = "Fay" , 
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = z
	)


atus_design <-
	update(
		atus_design ,
		any_care = as.numeric( tuactdur24_3 > 0 ) ,
		age_category = 
			factor( 
				1 + findInterval( teage , c( 18 , 35 , 65 ) ) , 
				labels = c( "under 18" , "18 - 34" , "35 - 64" , "65 or older" ) 
			)
	)
# caring for and helping household members row
# which we know is top level 03 from
# http://www.bls.gov/tus/lexiconnoex2012.pdf

sum( weights( atus_design , "sampling" ) != 0 )

svyby( ~ one , ~ age_category , atus_design , unwtd.count )
svytotal( ~ one , atus_design )

svyby( ~ one , ~ age_category , atus_design , svytotal )
svymean( ~ tuactdur24_1 , atus_design )

svyby( ~ tuactdur24_1 , ~ age_category , atus_design , svymean )
svymean( ~ tesex , atus_design )

svyby( ~ tesex , ~ age_category , atus_design , svymean )
svytotal( ~ tuactdur24_1 , atus_design )

svyby( ~ tuactdur24_1 , ~ age_category , atus_design , svytotal )
svytotal( ~ tesex , atus_design )

svyby( ~ tesex , ~ age_category , atus_design , svytotal )
svyquantile( ~ tuactdur24_1 , atus_design , 0.5 )

svyby( 
	~ tuactdur24_1 , 
	~ age_category , 
	atus_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ tuactdur24_5 , 
	denominator = ~ tuactdur24_12 , 
	atus_design 
)
sub_atus_design <- subset( atus_design , tuactdur24_15 > 0 )
svymean( ~ tuactdur24_1 , sub_atus_design )
this_result <- svymean( ~ tuactdur24_1 , atus_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ tuactdur24_1 , 
		~ age_category , 
		atus_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( atus_design )
svyvar( ~ tuactdur24_1 , atus_design )
# SRS without replacement
svymean( ~ tuactdur24_1 , atus_design , deff = TRUE )

# SRS with replacement
svymean( ~ tuactdur24_1 , atus_design , deff = "replace" )
svyciprop( ~ any_care , atus_design ,
	method = "likelihood" )
svyttest( tuactdur24_1 ~ any_care , atus_design )
svychisq( 
	~ any_care + tesex , 
	atus_design 
)
glm_result <- 
	svyglm( 
		tuactdur24_1 ~ any_care + tesex , 
		atus_design 
	)

summary( glm_result )
library(srvyr)
atus_srvyr_design <- as_survey( atus_design )
atus_srvyr_design %>%
	summarize( mean = survey_mean( tuactdur24_1 ) )

atus_srvyr_design %>%
	group_by( age_category ) %>%
	summarize( mean = survey_mean( tuactdur24_1 ) )

}
