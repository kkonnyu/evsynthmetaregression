setwd("~/Dropbox/PhD/8-Thesis/Publications/2-Methods/Analysis")

# libraries
library(lattice)
library(coda)
library(rjags)

how_many_runs <- 100000

model <- jags.model("04_model_Model_3.bug", data=read.jagsdata("03_data_jags_Model_3.R"), n.chains=3)

update(model, how_many_runs)

posteriors <- coda.samples(model, c("mu_bar","tau", "rk", "p.crossval", "xmu_posterior_combo_1", "xdif_posterior_combo_1", "xbase_posterior_combo_1", "xmu_posterior_combo_2", "xdif_posterior_combo_2", "xbase_posterior_combo_2","xmu_posterior_combo_3", "xdif_posterior_combo_3", "xbase_posterior_combo_3","xmu_posterior_predictive_combo_1", "xdif_posterior_predictive_combo_1", "xbase_posterior_predictive_combo_1","xmu_posterior_predictive_combo_2", "xdif_posterior_predictive_combo_2", "xbase_posterior_predictive_combo_2", "xmu_posterior_predictive_combo_3", "xdif_posterior_predictive_combo_3", "xbase_posterior_predictive_combo_3"), n.iter=how_many_runs, thin=1)  

summary(posteriors)
	
	results <- summary(posteriors)
		c1 <- results$quantiles[,1]
		c2 <- results$quantiles[,3]
		c3 <- results$quantiles[,5]
		
	#parameters
	summary.table <- data.frame(cbind(c1,c2,c3))
	write.table(summary.table, file = paste("04_model_Model_3",".csv", sep=""), quote = F, row.names = T)
	
	#postive predictive checks
	summary.table2 <- data.frame(cbind(results$statistics[11:251,1]))
	write.table(summary.table2, file= paste("04_model_Model_3_p_new", ".csv", sep=""), quote= F, row.names = T)
	
	#gelman diagnostics        
	gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)
	for (v in 1:nvar(posteriors)) {
   		gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
 		}
 	gelman_diag
 	diagnostics = paste("gelman_diag_", "04_model_Model_3", sep="")
 	write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)

 	#ranks
        best <- vector("list", 9)
 	
         for (i in 1:9) {
         	  j = 252
         	        best[[1]] <- c(as.array(posteriors[,j][1]), as.array(posteriors[,j][2]), as.array(posteriors[,j][3]))
         	        best[[2]] <- c(as.array(posteriors[,j+1][1]), as.array(posteriors[,j+1][2]), as.array(posteriors[,j+1][3]))
         	        best[[3]] <- c(as.array(posteriors[,j+2][1]), as.array(posteriors[,j+2][2]), as.array(posteriors[,j+2][3]))
         	        best[[4]] <- c(as.array(posteriors[,j+3][1]), as.array(posteriors[,j+3][2]), as.array(posteriors[,j+3][3]))
         	        best[[5]] <- c(as.array(posteriors[,j+4][1]), as.array(posteriors[,j+4][2]), as.array(posteriors[,j+4][3]))
         	        best[[6]] <- c(as.array(posteriors[,j+5][1]), as.array(posteriors[,j+5][2]), as.array(posteriors[,j+5][3]))
         	        best[[7]] <- c(as.array(posteriors[,j+6][1]), as.array(posteriors[,j+6][2]), as.array(posteriors[,j+6][3]))
         	        best[[8]] <- c(as.array(posteriors[,j+7][1]), as.array(posteriors[,j+7][2]), as.array(posteriors[,j+7][3]))
         	        best[[9]] <- c(as.array(posteriors[,j+8][1]), as.array(posteriors[,j+8][2]), as.array(posteriors[,j+8][3]))
         	}
         	
         	summary.table <- data.frame(cbind(best[[1]] , best[[2]] ,best[[3]] ,best[[4]] ,best[[5]] , best[[6]] ,
         	                                  best[[7]] , best[[8]] , best[[9]]))
         	
         	write.table(summary.table, file = "p_best_Model_3.csv")
         
 	
         #mean distribution - QI strategies	
         meanEF <- vector("list", 9)
         	for (i in 1:9) {
         	        j = 2
         	        meanEF[[1]] <- c(as.array(posteriors[,j][1]), as.array(posteriors[,j][2]), as.array(posteriors[,j][3]))
         	        meanEF[[2]] <- c(as.array(posteriors[,j+1][1]), as.array(posteriors[,j+1][2]), as.array(posteriors[,j+1][3]))
         	        meanEF[[3]] <- c(as.array(posteriors[,j+2][1]), as.array(posteriors[,j+2][2]), as.array(posteriors[,j+2][3]))
         	        meanEF[[4]] <- c(as.array(posteriors[,j+3][1]), as.array(posteriors[,j+3][2]), as.array(posteriors[,j+3][3]))
         	        meanEF[[5]] <- c(as.array(posteriors[,j+4][1]), as.array(posteriors[,j+4][2]), as.array(posteriors[,j+4][3]))
         	        meanEF[[6]] <- c(as.array(posteriors[,j+5][1]), as.array(posteriors[,j+5][2]), as.array(posteriors[,j+5][3]))
         	        meanEF[[7]] <- c(as.array(posteriors[,j+6][1]), as.array(posteriors[,j+6][2]), as.array(posteriors[,j+6][3]))
         	        meanEF[[8]] <- c(as.array(posteriors[,j+7][1]), as.array(posteriors[,j+7][2]), as.array(posteriors[,j+7][3]))
         	        meanEF[[9]] <- c(as.array(posteriors[,j+8][1]), as.array(posteriors[,j+8][2]), as.array(posteriors[,j+8][3]))
         	}
         	
         	summary.table <- data.frame(cbind(meanEF[[1]] , meanEF[[2]] ,meanEF[[3]] ,meanEF[[4]] ,meanEF[[5]] , meanEF[[6]] ,
         	                                  meanEF[[7]] , meanEF[[8]] , meanEF[[9]]))
         	write.table(summary.table, file = "posteriors_mean_Model_3.csv")
         	
        #mean distribution - post mean in absence of treatment	
        meanEF_baseline <- c(as.array(posteriors[,1][1]), as.array(posteriors[,1][2]), as.array(posteriors[,1][3]))
        summary.table <- data.frame(cbind(meanEF_baseline))
        write.table(summary.table, file = "posteriors_mean_baseline_Model_3.csv")
         
        #tau distribution - QI strategies - HbA1c	
        tau <- vector("list", 9)
        for (i in 1:9) {
                j = 261
                tau[[1]] <- c(as.array(posteriors[,j][1]), as.array(posteriors[,j][2]), as.array(posteriors[,j][3]))
                tau[[2]] <- c(as.array(posteriors[,j+1][1]), as.array(posteriors[,j+1][2]), as.array(posteriors[,j+1][3]))
                tau[[3]] <- c(as.array(posteriors[,j+2][1]), as.array(posteriors[,j+2][2]), as.array(posteriors[,j+2][3]))
                tau[[4]] <- c(as.array(posteriors[,j+3][1]), as.array(posteriors[,j+3][2]), as.array(posteriors[,j+3][3]))
                tau[[5]] <- c(as.array(posteriors[,j+4][1]), as.array(posteriors[,j+4][2]), as.array(posteriors[,j+4][3]))
                tau[[6]] <- c(as.array(posteriors[,j+5][1]), as.array(posteriors[,j+5][2]), as.array(posteriors[,j+5][3]))
                tau[[7]] <- c(as.array(posteriors[,j+6][1]), as.array(posteriors[,j+6][2]), as.array(posteriors[,j+6][3]))
                tau[[8]] <- c(as.array(posteriors[,j+7][1]), as.array(posteriors[,j+7][2]), as.array(posteriors[,j+7][3]))
                tau[[9]] <- c(as.array(posteriors[,j+8][1]), as.array(posteriors[,j+8][2]), as.array(posteriors[,j+8][3]))
        }
        
        summary.table <- data.frame(cbind(tau[[1]] , tau[[2]] ,tau[[3]] ,tau[[4]] ,tau[[5]] , tau[[6]] ,
                                          tau[[7]] , tau[[8]] , tau[[9]]))
        write.table(summary.table, file = "posteriors_tau_Model_3.csv")
        
        #tau distribution - tau for post mean in absence of treatment	
        tau_baseline <- c(as.array(posteriors[,261][1]), as.array(posteriors[,261][2]), as.array(posteriors[,261][3]))
        summary.table <- data.frame(cbind(tau_baseline))
        write.table(summary.table, file = "posteriors_tau_baseline_Model_3.csv")
 	
 	
 