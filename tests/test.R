machine_specific_replacements <- 
	list( 
		
		# replace the folder path on macnix
		c( 'path.expand( \"~\" ) , \"ATUS\"' , paste0( '"' , getwd() , '"' ) ) ,
		
		# change other things in the script to be run
		c( "hello" , "howdy" )
	)
if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

source( lodown::syntaxtractor( "atus" , replacements = machine_specific_replacements , setup_test = "test" ) , echo = TRUE )
