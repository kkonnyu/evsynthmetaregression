/* pass data to R/JAGS */

cap cd "/Users/Kristin/Dropbox/PhD/8-Thesis/Publications/2-Methods/Analysis"

import excel using "00_analysis_data.xlsx", sheet("HbA1c") firstrow clear
/*238 arms from 119 studies*/

drop if RefID >= 9000

keep Author_year   RefID    Cluster   Number_of_clusters   Post_arms_need_cluster  ICC_present ICCGroup*  ///
		NGroup*  PreHbA1cmeanGroup* PreHbA1cSDGroup* PreHbA1cSEGroup* ///
		PostNGroup* PostHbA1cmeanGroup* PostHbA1cSDGroup* PostHbA1cSEGroup*

sort RefID

egen study_number = group( RefID )

destring Number_of_clusters ICC_present ICCGroup* ///
			NGroup* PreHbA1cmeanGroup* PreHbA1cSDGroup* PreHbA1cSEGroup* ///
			PostNGroup* PostHbA1cmeanGroup* PostHbA1cSDGroup* PostHbA1cSEGroup*, force replace

reshape long  ICCGroup NGroup PreHbA1cmeanGroup PreHbA1cSDGroup PreHbA1cSEGroup ///
				PostNGroup  PostHbA1cmeanGroup PostHbA1cSDGroup PostHbA1cSEGroup, i(study_number) j(Arm)

drop if PostNGroup == .   /*0 observations deleted*/
drop if PreHbA1cmeanGroup == . /*10 observations deleted*/



/*baseline SE*/
replace PreHbA1cSEGroup = PreHbA1cSDGroup  / sqrt(NGroup) if PreHbA1cSEGroup == . 


/*follow-up SE*/
replace PostHbA1cSEGroup = PostHbA1cSDGroup  / sqrt(PostNGroup) if PostHbA1cSEGroup == . 

/*variables to fix cluster trials*/
generate EffectiveNGroup = PostNGroup 

/*to make pairwise data*/
sort Arm RefID

/*drop Arm 2, in 3-arm studies*/
drop if Arm == 2 & RefID  == 1585
drop if Arm == 2 & RefID  == 5083
drop if Arm == 2 & RefID  == 5774

/*drop Arm 2 and 3, in 4-arm studies*/
drop if Arm == 2 & RefID  == 2264
drop if Arm == 3 & RefID  == 2264
drop if Arm == 2 & RefID  == 5870
drop if Arm == 3 & RefID  == 5870
drop if Arm == 2 & RefID  == 6253
drop if Arm == 3 & RefID  == 6253
drop if Arm == 2 & RefID  == 7503
drop if Arm == 3 & RefID  == 7503
drop if Arm == 2 & RefID  == 8396
drop if Arm == 3 & RefID  == 8396

sort Arm

replace Arm = 2 if Arm > 2



bysort study_number  : egen total_n = sum(PostNGroup) 

generate avg_cluster_size = total_n / Number_of_clusters if Cluster == "Y"



/*to parse data for model analysis bins, based on trial desing (patient vs. cluster RCT) and missing data (SE and ICC)*/
/****************************/
/*      ANALYSIS BIN 1      */
/*       SE+, patient       */
/****************************/
/*leave alone, send to model as is*/
generate Analysis_bin = 1 


/****************************/
/*      ANALYSIS BIN 2      */
/*     SE+, cluster adj     */
/****************************/
/*leave alone, send to model as is*/
replace Analysis_bin = 2 if Post_arms_need_cluster == "N"

sort Analysis_bin RefID Arm


/****************************/
/*      ANALYSIS BIN 3      */
/* SE+, cluster un_adj, ICC+*/
/****************************/
/*make adjustments here, and send corrected data to R*/
replace Analysis_bin = 3 if Post_arms_need_cluster == "Y" & ICCGroup != . & PostHbA1cSEGroup !=.

generate design_effect = 1 + (avg_cluster_size - 1) * ICCGroup if Analysis_bin == 3

replace EffectiveNGroup = PostNGroup / design_effect  if Analysis_bin == 3

sort Analysis_bin RefID Arm

/*for studies with unadjusted SE, need to get unadjusted SD to recalculate adjusted SE using effective n*/
replace PostHbA1cSDGroup = PostHbA1cSEGroup * sqrt(PostNGroup) if Analysis_bin == 3 & PostHbA1cSDGroup == . 

replace PostHbA1cSEGroup = PostHbA1cSDGroup  / sqrt(EffectiveNGroup) if Analysis_bin == 3


/****************************/
/*      ANALYSIS BIN 4      */
/* SE+, cluster un_adj, ICC-  */
/****************************/
/*correct in R*/
replace Analysis_bin = 4 if Post_arms_need_cluster == "Y" & PostHbA1cSEGroup != . & ICCGroup == . 

sort Analysis_bin RefID Arm

/****************************/
/*      ANALYSIS BIN 5      */
/* 		 SE-, patient       */
/****************************/
/*correct in R*/
replace Analysis_bin = 5 if Cluster == "N" & PostHbA1cSEGroup == . 

sort Analysis_bin RefID Arm

/****************************/
/*      ANALYSIS BIN 6      */
/*  SE-,cluster un_adj,ICC+ */
/****************************/
/*correct in R*/
replace Analysis_bin = 6 if PostHbA1cSEGroup == . & Post_arms_need_cluster == "Y" & ICCGroup != . 

sort Analysis_bin RefID Arm

/****************************/
/*      ANALYSIS BIN 7      */
/*  SE-,cluster un_adj,ICC- */
/****************************/
/*correct in R*/
replace Analysis_bin = 7 if PostHbA1cSEGroup == . & Post_arms_need_cluster == "Y" & ICCGroup == . 

sort Analysis_bin RefID Arm


bysort study_number  : egen n_arms = max(Arm)


save "analysis_Outcome_HbA1c_Model_2.dta", replace

summarize PreHbA1cmeanGroup PreHbA1cSDGroup PreHbA1cSEGroup PostHbA1cmeanGroup PostHbA1cSDGroup PostHbA1cSEGroup,  separator (3)



/* QI coding */
import excel using "00_analysis_data.xlsx", sheet("QI_codes") firstrow clear

drop if RefID >= 9000

keep  Author_year  RefID  Arm 	AF	 CM	 TC	 EPR   CE	CR	 FR	  PE   PSM	 PR	  CQI	FI

/*to make pairwise data*/
sort Arm RefID

/*drop Arm 2, in 3-arm HbA1c studies*/
drop if Arm == 2 & RefID  == 1585
drop if Arm == 2 & RefID  == 5083
drop if Arm == 2 & RefID  == 5774

/*drop Arm 2, in 3-arm non-Hba1c studies to support merging*/
drop if Arm == 2 & RefID  == 3549
drop if Arm == 2 & RefID  == 6546

/*drop Arm 2 and 3, in 4-arm studies*/
drop if Arm == 2 & RefID  == 2264
drop if Arm == 3 & RefID  == 2264
drop if Arm == 2 & RefID  == 5870
drop if Arm == 3 & RefID  == 5870
drop if Arm == 2 & RefID  == 6253
drop if Arm == 3 & RefID  == 6253
drop if Arm == 2 & RefID  == 7503
drop if Arm == 3 & RefID  == 7503
drop if Arm == 2 & RefID  == 8396
drop if Arm == 3 & RefID  == 8396

sort Arm

replace Arm = 2 if Arm > 2


save "analysis_Intervention_QI_Model_2.dta" , replace

use "analysis_Outcome_HbA1c_Model_2.dta" , clear

merge 1:1 RefID Arm using "analysis_Intervention_QI_Model_2.dta"

sort _merge

drop if _merge == 1

drop if _merge == 2

drop _merge

/*
drop if RefID == 2915
drop if RefID == 5101
drop if RefID == 5943
*/

sort Analysis_bin  study_number  Arm

keep study_number	Arm		Author_year		n_arms		RefID		Analysis_bin		avg_cluster_size		ICCGroup	///
	 NGroup 	PreHbA1cmeanGroup 	PreHbA1cSDGroup 	PreHbA1cSEGroup		/// 
	 EffectiveNGroup 	PostHbA1cmeanGroup   PostHbA1cSEGroup  ///
	 AF	 CM	 TC	 EPR   CE	CR	 FR	  PE   PSM	 PR	  CQI	FI n_arms

egen inc_study_number = group( Analysis_bin RefID )			
																				
generate QI_other = 1 if      AF == 1 | CR ==1 | CQI == 1 | FI == 1 
		
replace QI_other = 0 if QI_other == . 

foreach component in AF ///
							CM TC EPR ///
							CE CR FR ///
							PE PSM PR ///
							CQI FI QI_other {
							
							tab `component'
							
}
///

reshape wide ICCGroup 		NGroup 		PreHbA1cmeanGroup 		PreHbA1cSDGroup 		PreHbA1cSEGroup ///
			 EffectiveNGroup 	PostHbA1cmeanGroup  	PostHbA1cSEGroup ///
			 AF	 CM	 TC	 EPR   CE	CR	 FR	  PE   PSM	 PR	  CQI	FI QI_other, i(inc_study_number) j(Arm)								
															
outsheet inc_study_number  	avg_cluster_size  	ICCGroup* ///
		 NGroup* 	PreHbA1cmeanGroup* 		PreHbA1cSDGroup* 	PreHbA1cSEGroup*  ///
		 EffectiveNGroup* 	PostHbA1cmeanGroup* 	PostHbA1cSEGroup*  ///
							AF* CM* TC* EPR* ///
							CE* CR* FR* ///
							PE* PSM* PR* ///
							CQI* FI* QI_other* ///
							n_arms using "cleaned_data_Model_2.txt" , replace comma
							
						


