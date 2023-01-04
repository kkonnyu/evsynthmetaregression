setwd("~/Dropbox/PhD/8-Thesis/Publications/2-Methods/Analysis")

# libraries
library(lattice)
library(coda)
library(rjags)

how_many_runs <- 100000

model <- jags.model("04_model_Model_3b.bug", data=read.jagsdata("03_data_jags_Model_3b.R"), n.chains=3)

update(model, how_many_runs)

posteriors <- coda.samples(model, c("mu_bar", "p.crossval", "z2_CM_U_Int", "z2_CM_U_NOInt", "z2_CM_Dif_U", "z2_CM_C_Int", "z2_CM_C", "z2_CM_Dif_C", "z2_Diff_of_diff_CM", "z3_TC_U_Int", "z3_TC_U_NOInt", "z3_TC_Dif_U", "z3_TC_C_Int", "z3_TC_C", "z3_TC_Dif_C", "z3_Diff_of_diff_TC", "z4_EPR_U_Int", "z4_EPR_U_NOInt", "z4_EPR_Dif_U", "z4_EPR_C_Int", "z4_EPR_C", "z4_EPR_Dif_C", "z4_Diff_of_diff_EPR", "z5_CE_U_Int", "z5_CE_U_NOInt", "z5_CE_Dif_U", "z5_CE_C_Int", "z5_CE_C", "z5_CE_Dif_C", "z5_Diff_of_diff_CE", "z6_FR_U_Int", "z6_FR_U_NOInt", "z6_FR_Dif_U", "z6_FR_C_Int", "z6_FR_C", "z6_FR_Dif_C", "z6_Diff_of_diff_FR", "z7_PE_U_Int", "z7_PEU_NOInt", "z7_PE_Dif_U", "z7_PE_C_Int", "z7_PE_C", "z7_PE_Dif_C", "z7_Diff_of_diff_PE", "z8_PSM_U_Int", "z8_PSM_U_NOInt", "z8_PSM_Dif_U", "z8_PSM_C_Int", "z8_PSM_C", "z8_PSM_Dif_C", "z8_Diff_of_diff_PSM", "z9_PR_U_Int", "z9_PR_U_NOInt", "z9_PR_Dif_U", "z9_PR_C_Int", "z9_PR_C", "z9_PR_Dif_C", "z9_Diff_of_diff_PR", "z10_Other_U_Int", "z10_Other_U_NOInt", "z10_Other_Dif_U", "z10_Other_C_Int", "z10_Other_C", "z10_Other_Dif_C", "z10_Diff_of_diff_Other"), n.iter=how_many_runs, thin=1)  

summary(posteriors)
	
	results <- summary(posteriors)
		c1 <- results$quantiles[,1]
		c2 <- results$quantiles[,3]
		c3 <- results$quantiles[,5]
	
	#parameters	
	summary.table <- data.frame(cbind(c1,c2,c3))	
	write.table(summary.table, file = paste("04_model_Model_3b",".csv", sep=""), quote = F, row.names = T)

	#positive predictive checks
	summary.table2 <- data.frame(cbind(results$statistics[21:261,1]))
	write.table(summary.table2, file= paste("04_model_Model_3b_p_new", ".csv", sep=""), quote= F, row.names = T)
	
	#gelman diagnositics
	gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
	for (v in 1:nvar(posteriors)) {
   		gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
 		}
 	gelman_diag
 	diagnostics = paste("gelman_diag_", "04_model_Model_3b", sep="")
 	write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)




