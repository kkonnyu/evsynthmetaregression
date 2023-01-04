setwd("~/Dropbox/PhD/8-Thesis/Publications/2-Methods/Analysis")

# libraries
library(lattice)
library(coda)
library(rjags)

how_many_runs <- 100000

model <- jags.model("04_model_Model_2.bug", data=read.jagsdata("03_data_jags_Model_2.R"), n.chains=3)

update(model, how_many_runs)

posteriors <- coda.samples(model, c("mu_bar", "tau"), n.iter=how_many_runs, thin=1)  

summary(posteriors)
	
	results <- summary(posteriors)
		c1 <- results$quantiles[,1]
		c2 <- results$quantiles[,3]
		c3 <- results$quantiles[,5]
		
	summary.table <- data.frame(cbind(c1,c2,c3))	
	
	write.table(summary.table, file = paste("04_model_Model_2",".csv", sep=""), quote = F, row.names = T)



	gelman_diag <- matrix(NA, nrow=nvar(posteriors), ncol=2)

	for (v in 1:nvar(posteriors)) {
   		gelman_diag[v,] <- gelman.diag(posteriors[,v])$psrf
 		}
 	
 	gelman_diag
 	
 	diagnostics = paste("gelman_diag_", "04_model_Model_2", sep="")
 	
 	write.table(gelman_diag, file = paste(diagnostics,".csv", sep=""), quote = F, row.names = T)


