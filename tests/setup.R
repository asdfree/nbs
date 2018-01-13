if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "nbs" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NBS microdata files
nbs_cat <-
	get_catalog( "nbs" ,
		output_dir = file.path( getwd() ) )

# 2010 only
nbs_cat <- subset( nbs_cat , this_round == 4 )
# download the microdata to your local computer


library(survey)

nbs_df <- readRDS( file.path( getwd() , "round 04.rds" ) )

nbs_design <- 
	svydesign( 
		~ a_psu_pub , 
		strata = ~ a_strata , 
		data = nbs_df , 
		weights = ~ wtr4_ben 
	)
nbs_design <- 
	update( 
		nbs_design , 
		
		male = as.numeric( orgsampinfo_sex == 1 ) ,
		
		age_categories = 
			factor( 
				c_intage_pub ,
				labels = 
					c( "18-25" , "26-40" , "41-55" , "56 and older" )
			)
		
	)
sum( weights( nbs_design , "sampling" ) != 0 )

svyby( ~ one , ~ age_categories , nbs_design , unwtd.count )
svytotal( ~ one , nbs_design )

svyby( ~ one , ~ age_categories , nbs_design , svytotal )
svymean( ~ n_totssbenlastmnth_pub , nbs_design )

svyby( ~ n_totssbenlastmnth_pub , ~ age_categories , nbs_design , svymean )
svymean( ~ c_hhsize_pub , nbs_design )

svyby( ~ c_hhsize_pub , ~ age_categories , nbs_design , svymean )
svytotal( ~ n_totssbenlastmnth_pub , nbs_design )

svyby( ~ n_totssbenlastmnth_pub , ~ age_categories , nbs_design , svytotal )
svytotal( ~ c_hhsize_pub , nbs_design )

svyby( ~ c_hhsize_pub , ~ age_categories , nbs_design , svytotal )
svyquantile( ~ n_totssbenlastmnth_pub , nbs_design , 0.5 )

svyby( 
	~ n_totssbenlastmnth_pub , 
	~ age_categories , 
	nbs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE 
)
svyratio( 
	numerator = ~ n_ssilastmnth_pub , 
	denominator = ~ n_totssbenlastmnth_pub , 
	nbs_design 
)
sub_nbs_design <- subset( nbs_design , c_curmedicare == 1 )
svymean( ~ n_totssbenlastmnth_pub , sub_nbs_design )
this_result <- svymean( ~ n_totssbenlastmnth_pub , nbs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ n_totssbenlastmnth_pub , 
		~ age_categories , 
		nbs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nbs_design )
svyvar( ~ n_totssbenlastmnth_pub , nbs_design )
# SRS without replacement
svymean( ~ n_totssbenlastmnth_pub , nbs_design , deff = TRUE )

# SRS with replacement
svymean( ~ n_totssbenlastmnth_pub , nbs_design , deff = "replace" )
svyciprop( ~ male , nbs_design ,
	method = "likelihood" )
svyttest( n_totssbenlastmnth_pub ~ male , nbs_design )
svychisq( 
	~ male + c_hhsize_pub , 
	nbs_design 
)
glm_result <- 
	svyglm( 
		n_totssbenlastmnth_pub ~ male + c_hhsize_pub , 
		nbs_design 
	)

summary( glm_result )
library(srvyr)
nbs_srvyr_design <- as_survey( nbs_design )
nbs_srvyr_design %>%
	summarize( mean = survey_mean( n_totssbenlastmnth_pub ) )

nbs_srvyr_design %>%
	group_by( age_categories ) %>%
	summarize( mean = survey_mean( n_totssbenlastmnth_pub ) )

