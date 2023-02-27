# social safety net
# poverty acrobatics
# trap or trampoline
library(haven)

zip_tf <- tempfile()

zip_url <- "https://www.ssa.gov/disabilityresearch/documents/R7NBSPUF_STATA.zip"
	
download.file( zip_url , zip_tf , mode = 'wb' )

nbs_tbl <- read_stata( zip_tf )

nbs_df <- data.frame( nbs_tbl )

names( nbs_df ) <- tolower( names( nbs_df ) )

nbs_df[ , 'one' ] <- 1
# nbs_fn <- file.path( path.expand( "~" ) , "NBS" , "this_file.rds" )
# saveRDS( nbs_df , file = nbs_fn , compress = FALSE )
# nbs_df <- readRDS( nbs_fn )
options( survey.lonely.psu = "adjust" )

library(survey)

# representative beneficiary sample
nbs_design <-
	svydesign(
		id = ~ r7_a_psu_pub , 
		strata = ~ r7_a_strata , 
		weights = ~ r7_wtr7_ben , 
		data = subset( nbs_df , r7_wtr7_ben > 0 ) 
	)
	
# cross-sectional successful worker sample
nbs_design <- 
	svydesign(
		id = ~ r7_a_psu_pub , 
		strata = ~ r7_a_strata , 
		weights = ~ r7_wtr7_cssws , 
		data = subset( nbs_df , r7_wtr7_cssws > 0 ) 
	)
	
# longitudinal successful worker sample
lngsws_design <-
	svydesign(
		id = ~ r7_a_psu_pub , 
		strata = ~ r7_a_strata , 
		weights = ~ r7_wtr7_lngsws , 
		data = subset( nbs_df , r7_wtr7_lngsws > 0 ) 
	)
	
nbs_design <- 
	update( 
		nbs_design , 
		
		male = as.numeric( r7_orgsampinfo_sex == 1 ) ,
		
		age_categories = 
			factor( 
				r7_c_intage_pub ,
				labels = 
					c( "18-25" , "26-40" , "41-55" , "56 and older" )
			)
		
	)
sum( weights( nbs_design , "sampling" ) != 0 )

svyby( ~ one , ~ age_categories , nbs_design , unwtd.count )
svytotal( ~ one , nbs_design )

svyby( ~ one , ~ age_categories , nbs_design , svytotal )
svymean( ~ r7_n_totssbenlastmnth_pub , nbs_design )

svyby( ~ r7_n_totssbenlastmnth_pub , ~ age_categories , nbs_design , svymean )
svymean( ~ r7_c_hhsize_pub , nbs_design )

svyby( ~ r7_c_hhsize_pub , ~ age_categories , nbs_design , svymean )
svytotal( ~ r7_n_totssbenlastmnth_pub , nbs_design )

svyby( ~ r7_n_totssbenlastmnth_pub , ~ age_categories , nbs_design , svytotal )
svytotal( ~ r7_c_hhsize_pub , nbs_design )

svyby( ~ r7_c_hhsize_pub , ~ age_categories , nbs_design , svytotal )
svyquantile( ~ r7_n_totssbenlastmnth_pub , nbs_design , 0.5 )

svyby( 
	~ r7_n_totssbenlastmnth_pub , 
	~ age_categories , 
	nbs_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ r7_n_ssilastmnth_pub , 
	denominator = ~ r7_n_totssbenlastmnth_pub , 
	nbs_design 
)
sub_nbs_design <- subset( nbs_design , r7_c_curmedicare == 1 )
svymean( ~ r7_n_totssbenlastmnth_pub , sub_nbs_design )
this_result <- svymean( ~ r7_n_totssbenlastmnth_pub , nbs_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ r7_n_totssbenlastmnth_pub , 
		~ age_categories , 
		nbs_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nbs_design )
svyvar( ~ r7_n_totssbenlastmnth_pub , nbs_design )
# SRS without replacement
svymean( ~ r7_n_totssbenlastmnth_pub , nbs_design , deff = TRUE )

# SRS with replacement
svymean( ~ r7_n_totssbenlastmnth_pub , nbs_design , deff = "replace" )
svyciprop( ~ male , nbs_design ,
	method = "likelihood" )
svyttest( r7_n_totssbenlastmnth_pub ~ male , nbs_design )
svychisq( 
	~ male + r7_c_hhsize_pub , 
	nbs_design 
)
glm_result <- 
	svyglm( 
		r7_n_totssbenlastmnth_pub ~ male + r7_c_hhsize_pub , 
		nbs_design 
	)

summary( glm_result )
library(srvyr)
nbs_srvyr_design <- as_survey( nbs_design )
nbs_srvyr_design %>%
	summarize( mean = survey_mean( r7_n_totssbenlastmnth_pub ) )

nbs_srvyr_design %>%
	group_by( age_categories ) %>%
	summarize( mean = survey_mean( r7_n_totssbenlastmnth_pub ) )

r3_tf <- tempfile()

r3_url <- "https://www.ssa.gov/disabilityresearch/documents/nbsr3pufstata.zip"
	
download.file( r3_url , r3_tf , mode = 'wb' )

r3_tbl <- read_stata( r3_tf )

r3_df <- data.frame( r3_tbl )

names( r3_df ) <- tolower( names( r3_df ) )

r3_design <- 
	svydesign(
		id = ~ r3_a_psu_pub , 
		strata = ~ r3_a_strata , 
		weights = ~ r3_wtr3_ben , 
		data = subset( r3_df , r3_wtr3_ben > 0 ) 
	)
	
r3_design <-
	update(
		r3_design ,
		
		benefit_type =
			factor(
				r3_orgsampinfo_bstatus ,
				levels = c( 2 , 3 , 1 ) ,
				labels = c( 'ssdi' , 'concurrent' , 'ssi' )
			)

	)

ex_4 <-
	data.frame(
		variable_label =
			c( 'coping with stress' , 'concentrating' , 
			'getting around outside of the home' , 
			'shopping for personal items' , 'preparing meals' , 
			'getting into or out of bed' , 'bathing or dressing' , 
			'getting along with others' , 
			'getting around inside the house' , 'eating' ) ,
		variable_name =
			c( "r3_i60_i" , "r3_i59_i" , "r3_i47_i" , "r3_i53_i" , 
			"r3_i55_i" , "r3_i49_i" , "r3_i51_i" , "r3_i61_i" , 
			"r3_i45_i" , "r3_i57_i" ) ,
		overall_percentage =
			c( 61 , 58 , 47 , 39 , 37 , 34 , 30 , 27 , 23 , 14 ) ,
		di_only_percentage =
			c( 60 , 54 , 47 , 36 , 35 , 36 , 30 , 23 , 24 , 13 ) ,
		concurrent_percentage =
			c( 63 , 63 , 47 , 43 , 41 , 34 , 33 , 31 , 23 , 15 ) ,
		concurrent_vs_di =
			c( F , T , F , F , F , F , F , T , F , F ) ,
		ssi_percentage =
			c( 61 , 62 , 47 , 40 , 39 , 33 , 29 , 31 , 22 , 15 ) ,
		ssi_vs_di =
			c( F , T , F , F , F , F , F , T , F , F )
	)
		

for( i in seq( nrow( ex_4 ) ) ){

	this_formula <- as.formula( paste( "~" , ex_4[ i , 'variable_name' ] ) )

	overall_percent <- svymean( this_formula , r3_design )
	
	stopifnot( 100 * round( coef( overall_percent ) , 2 ) == ex_4[ i , 'overall_percent' ] )
	
	benefit_percent <- svyby( this_formula , ~ benefit_type , r3_design , svymean )
	
	stopifnot(
		all.equal( 
			100 * as.numeric( round( coef( benefit_percent ) , 2 ) ) , 
			as.numeric( ex_4[ i , grep( 'percentage' , names( ex_4 ) ) ] )
		)
	)
	
	ttest_formula <- as.formula( paste( ex_4[ i , 'variable_name' ] , "~ benefit_type" ) )
	
	ssdi_con_design <- subset( r3_design , benefit_type %in% c( 'ssdi' , 'concurrent' ) )
	
	con_ttest <- svyttest( ttest_formula , ssdi_con_design )

	stopifnot(
		all.equal( 
			as.logical( con_ttest$p.value < 0.05 ) , 
			as.logical( ex_4[ i , 'concurrent_vs_di' ] )
		)
	)
	
	ssdi_ssi_design <- subset( r3_design , benefit_type %in% c( 'ssdi' , 'ssi' ) )
	
	ssi_ttest <- svyttest( ttest_formula , ssdi_ssi_design )

	stopifnot(
		all.equal(
			as.logical( ssi_ttest$p.value < 0.05 ) , 
			as.logical( ex_4[ i , 'ssi_vs_di' ] ) 
		)
	)

}

