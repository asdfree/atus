if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

atus_cat <-
	get_catalog( "atus" ,
		output_dir = file.path( getwd() ) )

# sample 25% of the records
which_records <- sample( seq( nrow( atus_cat ) ) , round( nrow( atus_cat ) * 0.25 ) )

# always sample year == 2015
atus_cat <- unique( rbind( atus_cat[ which_records , ] , subset( atus_cat , year == 2015 ) ) )

lodown( "atus" , atus_cat )
