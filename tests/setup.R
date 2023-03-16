# don't judge me bruno
# eat one hour, sleep the rest
# it's my lazy day

atus_csv_import <-
	function( this_url ){
		
		this_tf <- tempfile()
		
		download.file( this_url , this_tf , mode = 'wb' )
		
		unzipped_files <- unzip( this_tf , exdir = tempdir() )
		
		this_dat <- grep( '\\.dat$' , unzipped_files , value = TRUE )
		
		this_df <- read.csv( this_dat )
		
		file.remove( c( this_tf , unzipped_files ) )
		
		names( this_df ) <- tolower( names( this_df ) )
		
		this_df
	}

act_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atusact-2021.zip" )

resp_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atusresp-2021.zip" )

rost_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atusrost-2021.zip" )

wgts_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atuswgts-2021.zip" )
act_df <- act_df[ c( 'tucaseid' , 'tutier1code' , 'tutier2code' , 'tuactdur24' ) ]

resp_df <- resp_df[ c( 'tucaseid' , 'tufinlwgt' , 'tulineno' ) ]

rost_df <- rost_df[ , c( 'tucaseid' , 'tulineno' , 'teage' , 'tesex' ) ]
act_df[ act_df[ , 'tutier1code' ] == 18 & act_df[ , 'tutier2code' ] == 99 , 'tutier1code' ] <- 50

act_df[ act_df[ , 'tutier1code' ] == 18 , 'tutier1code' ] <-
	act_df[ act_df[ , 'tutier1code' ] == 18 , 'tutier2code' ]
act_long_df <- aggregate( tuactdur24 ~ tucaseid + tutier1code , data = act_df , sum )

act_wide_df <-
	reshape( act_long_df , idvar = 'tucaseid' , timevar = 'tutier1code' , direction = 'wide' )

# for individuals not engaging in an activity category, replace missings with zero minutes
act_wide_df[ is.na( act_wide_df ) ] <- 0

# for all columns except the respondent identifier, convert minutes to hours
act_wide_df[ , -1 ] <- act_wide_df[ , -1 ] / 60
resp_act_df <- merge( resp_df , act_wide_df )

stopifnot( nrow( resp_act_df ) == nrow( resp_df ) )

resp_act_rost_df <- merge( resp_act_df , rost_df )

stopifnot( nrow( resp_act_rost_df ) == nrow( resp_df ) )

atus_df <- merge( resp_act_rost_df , wgts_df )

stopifnot( nrow( atus_df ) == nrow( resp_df ) )

# remove dots from column names
names( atus_df ) <- gsub( "\\." , "_" , names( atus_df ) )

atus_df[ , 'one' ] <- 1
# atus_fn <- file.path( path.expand( "~" ) , "ATUS" , "this_file.rds" )
# saveRDS( atus_df , file = atus_fn , compress = FALSE )
# atus_df <- readRDS( atus_fn )
library(survey)

atus_design <- 
	svrepdesign(
		weights = ~ tufinlwgt ,
		repweights = "finlwgt[0-9]" , 
		type = "Fay" , 
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		mse = TRUE ,
		data = atus_df
	)

# caring for and helping household members is top level 03 from the lexicon
# https://www.bls.gov/tus/lexicons/lexiconnoex2021.pdf

atus_design <-
	update(
		atus_design ,
		
		any_care = as.numeric( tuactdur24_3 > 0 ) ,
		
		tesex = factor( tesex , levels = 1:2 , labels = c( 'male' , 'female' ) ) ,
		
		age_category = 
			factor( 
				1 + findInterval( teage , c( 18 , 35 , 65 ) ) , 
				labels = c( "under 18" , "18 - 34" , "35 - 64" , "65 or older" ) 
			)
	)

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
	ci = TRUE 
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
hours_per_day_civilian_population <- svymean( ~ tuactdur24_3 , atus_design )

stopifnot( round( coef( hours_per_day_civilian_population ) , 2 ) == 0.47 )

percent_engaged_per_day <- svymean( ~ any_care , atus_design )

stopifnot( round( coef( percent_engaged_per_day ) , 3 ) == 0.217 )

hours_per_day_among_engaged <- svymean( ~ tuactdur24_3 , subset( atus_design , any_care ) )

stopifnot( round( coef( hours_per_day_among_engaged ) , 2 ) == 2.17 )
actsum07_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atussum_2007.zip" )
resp07_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atusresp_2007.zip" )
act07_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atusact_2007.zip" )
wgts07_df <- atus_csv_import( "https://www.bls.gov/tus/datafiles/atuswgts_2007.zip" )
television_per_person <-
	data.frame(
		tucaseid = actsum07_df[ , 'tucaseid' ] ,

		tuactdur24 = rowSums( actsum07_df[ , c( 't120303' , 't120304' ) ] )
	)

television_per_person <- 
	television_per_person[ television_per_person[ , 'tuactdur24' ] > 0 , ]
television_activity <- 
	subset( 
		act07_df , 
		tutier1code == 12 &
		tutier2code == 3 &
		tutier3code %in% 3:4
	)

television_activity_summed <-
		aggregate(
			tuactdur24 ~ tucaseid ,
			data = television_activity ,
			sum
		)
stopifnot(
	all( television_per_person[ , 'tucaseid' ] == television_activity_summed[ , 'tucaseid' ] )
)

stopifnot(
	all( television_per_person[ , 'tuactdur24' ] == television_activity_summed[ , 'tuactdur24' ] )
)
resp07_tpp_df <- 
	merge( 
		resp07_df[ , c( 'tucaseid' , 'tufinlwgt' ) ] , 
		television_per_person , 
		all.x = TRUE
	)

stopifnot( nrow( resp07_tpp_df ) == nrow( resp07_df ) )

# for individuals without television time, replace missings with zero minutes
resp07_tpp_df[ is.na( resp07_tpp_df[ , 'tuactdur24' ] ) , 'tuactdur24' ] <- 0

# convert minutes to hours
resp07_tpp_df[ , 'tuactdur24_hour' ] <- resp07_tpp_df[ , 'tuactdur24' ] / 60

atus07_df <- merge( resp07_tpp_df , wgts07_df )

stopifnot( nrow( atus07_df ) == nrow( resp07_df ) )
atus07_design <- 
	svrepdesign(
		weights = ~ tufinlwgt ,
		repweights = "finlwgt[0-9]" ,
		type = "Fay" ,
		rho = ( 1 - 1 / sqrt( 4 ) ) ,
		data = atus07_df
	)
result <- svymean( ~ tuactdur24_hour , atus07_design )

stopifnot( round( coef( result ) , 2 ) == 2.62 )
stopifnot( round( SE( result ) , 4 ) == 0.0293 )
