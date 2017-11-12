if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

atus_cat <-
	get_catalog( "atus" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( atus_cat ) ) / ceiling( nrow( atus_cat ) / 5 ) )

atus_cat <- unique( rbind( atus_cat[ record_categories == this_sample_break , ] , atus_cat[ atus_cat$directory == 2015 , ] ) )

lodown( "atus" , atus_cat )
