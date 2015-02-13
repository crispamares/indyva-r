# statistics.r - 
    # Created on 18/12/2013
    # @author: sgonzalez
	# Modified on 16/12/2014

	# En caso de ser necesario, instalar las librerias, copiando la siguiente linea.
	# install.packages(c("rzmq","fitdistrplus","rjson"));

	library("rzmq");
	library("rjson");
	library('fitdistrplus');
  
	if(!exists("JSONRPC.Protocol", mode="function")) source("jsonrpc.r");
  source("orderRecorder.r");

	#===============================================================================
	#   Public methods
	#===============================================================================


	srv_description <- function(){
	    cat('srv_description\n');
	    fn <- list(
	      compareAll = list(
	        params = list( c("list", "double array of arrays, list of samples of values"),
	                       c("list", "string array, names of variables to compare"),
	                       c("list", "string array, names of subsets"),
	                       c("type", "independent (i) or dependent data (d), default=i"),
	                       c("type_comparison", "string, possible values: two.sided | greater | less, default=two.sided")
	        ),
	        return = list(list(pvalue="double, p value of the test", 
	                           subsetA="string, subset name",
	                           subsetB="string, subset name",
	                           decision="string, indicating if test is rejected or not",
	                           test="string, type of test used",
	                           desc="string, description of the test",
	                           rejected="boolean, indicating if the test is rejected",
	                           warning="string, warning info about the execution",
	                           info="string, information about the execution")
	        )
	      ),
	      compare = list(
	        params = list( c("list", "double array of arrays, list of samples of values"),
	                       c("list", "string array, names of variables to compare"),
	                       c("list", "string array, names of subsets"),
	                       c("type", "independent (i) or dependent data (d), default=i"),
	                       c("type_comparison", "string, possible values: two.sided | greater | less, default=two.sided")
	        ),
	        return = list(pvalue="double, p value of the test", 
	                      decision="string, indicating if test is rejected or not",
	                      test="string, type of test used",
	                      desc="string, description of the test",
	                      rejected="boolean, indicating if the test is rejected",
	                      warning="string, warning info about the execution",
	                      info="string, information about the execution"
	        )
	      ),
	      statSort = list(
	        params = list( c("list", "double array of arrays, list of samples of values"),
	                       c("list", "string array, names of variables to compare"),
	                       c("list", "string array, names of subsets")
	        ),
	        return = list(
	          sorting = list(
	            order = "list(subset) in decreasing order. Subset is a string",
	            equals = "{subset : list(subset)} hashmap of subsets that come from shame distribution. Subset is a string"
	          ),  
	          tests = list(list(
	            subsetA="string, subset name",
	            subsetB="string, subset name",
	            pvalue="double, p value of the test", 
	            decision="string, indicating if test is rejected or not",
	            test="string, type of test used",
	            desc="string, description of the test",
	            rejected="boolean, indicating if the test is rejected",
	            warning="string, warning info about the execution",
	            info="string, information about the execution"))
	        )
	      ),
	      correlation = list(
	        params = list( c("list", "double array of arrays, list of 2 samples of values")
	        ),
	        return = list(value="double, correlation value", 
	                      type="string, type of correlation")
	      ),
	      #compareTwo = list(
	      #		params = list( c("list1", "double array, first list of values"),
	      #		       	       c("list2", "double array, second list of values"),
	      #			       c("type", "char, representing continuous data (c) or discrete data (d), default=c"),
	      #			       c("type_comparison", "string, possible values: two.sided | greater | less, default=two.sided")
	      #			       ),
	      #		return = list(ks="double, p value of the KS test", 
	      #		       	      wilcox="double, p value of the Wilcoxon test")
	      #),
	      basicStats = list(
	        params = list( c("list", "double array, list of values") ),
	        return = list(min="double, min value of the list",
	                      q1="double, first quartile value of the list",
	                      median="double, median value of the list",
	                      mean="double, mean value of the list",
	                      q3="double, third quartile value of the list",
	                      max="double, max value of the list",
	                      sd="double, std. deviation value of the list")
	      ),
	      distributionOf = list(
	        params = list( c("list", "double array, list of values"),
	                       c("type", "char, representing continuous data (c) or discrete data (d), default=c")
	        ),
	        return = list(dist="list of distibutions not rejected")
	      ),
	      getInfoDistribution = list(
	        params = list( c("list", "double array, list of values"),
	                       c("dist", "string, indicating the name of the distribution you want to fit with the list of values")
	        ),
	        return = list(chisq="double, p value of the ChiSq test",
	                      cramer.value="double, p value of the Von Cramer test",
	                      ad.value="double, p value of the Anderson Darling test",
	                      ks.value="double, p value of the KS test",
	                      cramer.test="string, Von Cramer test decission",
	                      ad.test="string, Anderson Darling test decission",
	                      ks.test="string, KS test decission",
	                      estimate="list of doubles, parameters estimation of the distribution",
	                      plot="svg image in text format, plots of fitting distribution")
	      )
	    );
	}


	#correlation between two samples
	correlation <- function(list){
		if (length(list) != 2){
		   return(-1);
		}else{
		   lista1 <- list[[1]];
		   lista2 <- list[[2]];
		   if (sigueNormal(lista1) == TRUE && sigueNormal(lista2) == TRUE){ # Parametrico
		      cat("Siguen una dist. Normal\n");
		      c = cor(lista1,lista2,method="pearson");
		      type <- 'Pearson correlation';
		   }else{ # No parametrico
		      cat("No sigue distribucion\n");
		      c = cor(lista1,lista2,method="spearman");
		      type <- 'Spearman correlation';
		   }
		   return(list(value=c,type=type));
		}
	}

	# compare several samples by 2
	compareAll <- function(list){
		if (length(list) < 3){ 
		  return(-1);
		}else{
		  lista_samples <- list[[1]];
		  lista_variables <- list[[2]];
		  lista_subsets <- list[[3]];
		  type_comparison <- "two.sided";
		  type_data <- "i";
		  if (list[[4]] != "i" && list[[4]] != "d") return(-1);
		  type_data <- list[[4]];
		  if (length(list) == 5){
		     if (list[[5]] != "two.sided" && list[[5]] != "greater" && list[[5]] != "less") return(-1);
		     type_comparison <- list[[5]];
	          }
		  result <- list();
		  idx <- 1;
		  cat('Num of samples:',length(lista_samples),'\n');
		  for (i in 1:(length(lista_samples)-1)){
		        for (j in (i+1):length(lista_samples)){
				    cat('Executing comparison between',i,'and',j,'...\n');
		        	    samples_aux <- list(lista_samples[[i]],lista_samples[[j]]);
			      	    variables_aux <- list(lista_variables[[i]],lista_variables[[j]]);
			      	    subsets_aux <- list(lista_subsets[[i]],lista_subsets[[j]]);
				    res <- compare(list(samples_aux,variables_aux,subsets_aux,type_data,type_comparison));
            res$subsetA <- lista_subsets[[i]];
				    res$subsetB <- lista_subsets[[j]];
				    result[[idx]] <- res;
				    idx <- idx + 1;
		        }
		  }
		  return(result);
		}
	}

	statSort <- function(list) {
    args <- list;
	  if (length(list) < 3) return(-1);
	  samples_list <- args[[1]];
	  variables_list <- args[[2]];
	  subsets_list <- args[[3]];  
    args[[4]] <- "i";
    
    record <- CreateOrderRecorder();
    
	  compare_types <- c("two.sided", "greater", "less");
	  relationships <- c("equals", "larger", "smaller");
	  
    results <- list();
    
    for (i in 1:3) {
      args[[5]] <- compare_types[i];
      
      message("compareAll - ", args[2:5]);
      
      compare_result <- compareAll(args);
      for (res in compare_result) {  
        if (! res$rejected && res$warning == "") { 
          results <- append(results, list(res));
          record$track(res$subsetA, res$subsetB, relationships[i]);
        }
      }
    }
    
    if (! record$isAllCorrect()) {return (-1)}
    
	  return (
	    list(  
	      sorting = list(
	        order = record$orderElems(),
	        equals = record$equals()
	      ),
	      tests = results)
	  );
	}
	
	# compare two or more samples
	compare <- function(list){
		if (length(list) < 3){ 
		  return(-1);
		}else{
		  lista_samples <- list[[1]];
		  lista_variables <- list[[2]];
		  lista_subsets <- list[[3]];
	      type_comparison <- "two.sided";
		  type_data <- "i";
		  if (list[[4]] != "i" && list[[4]] != "d") return(-1);
		  type_data <- list[[4]];
		  if (length(list) == 5){
		     if (list[[5]] != "two.sided" && list[[5]] != "greater" && list[[5]] != "less") return(-1);
		     type_comparison <- list[[5]];
	          }
		}
		warn <- "";
		info <- "";
		if (length(lista_samples) == 2){ # Dos muestras
		   cat("Numero de samples:2\n");
		   if (length(lista_samples[[1]]) < 50 || length(lista_samples[[2]]) < 50){
		   		warn <- 'Length of samples should be greater than 50 in order to correctly compare.';
		   }
		   tamMaximo <- min(length(lista_samples[[1]]),length(lista_samples[[2]]));
		   info <- paste('Selecting',tamMaximo,'values as population samples for comparison of each variable.');
		   sample_1 <- sample(lista_samples[[1]],size=tamMaximo,replace=FALSE);
		   sample_2 <- sample(lista_samples[[2]],size=tamMaximo,replace=FALSE);
		   if (sigueNormal(lista_samples[[1]]) == TRUE && sigueNormal(lista_samples[[2]]) == TRUE){ # Parametrico
		   	   cat("Siguen una dist. Normal\n");
		   	   if (type_data == "i"){ # Independientes
			      cat("Independientes\n");
		   	      r <- t.test(sample_1,sample_2,paired=FALSE,alternative=type_comparison);
			      pvalue <- r$p.value;
			      if (type_comparison == 'two.sided'){
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'and subset',lista_subsets[[2]] ,'come from SAME Distribution.');
			      	 }else{ 
			      	 	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'and sample',lista_variables[[2]],'of subset',lista_subsets[[2]] ,'come from SAME Distribution.');
			      	 }
			      }else if (type_comparison == 'greater'){
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'IS GREATER THAN subset',lista_subsets[[2]]);
			      	 }else{ 
				      	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'IS GREATER THAN sample',lista_variables[[2]],'of subset',lista_subsets[[2]],'.');
			      	 }
			      	 # desc <- 'The hypothesis evaluated in the test affirms that the first sample is greater than the second sample.'
			      }else{
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'IS LOWER THAN subset',lista_subsets[[2]]);
			      	 }else{ 
				      	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'IS LOWER THAN sample',lista_variables[[2]],'of subset',lista_subsets[[2]],'.');
			      	 }
			      	 # desc <- 'The hypothesis evaluated in the test affirms that the first sample is lower than the second sample.'
			      }
			      if (pvalue <= 0.05){
					 if (pvalue < 0.0001){
					    dec <- '*** hypothesis rejected';
		                            rejected <- TRUE;
					 }else if (pvalue < 0.001){
					    dec <- '** hypothesis rejected';
		                            rejected <- TRUE;
					 }else{
					    dec <- '* hypothesis rejected';
		                            rejected <- TRUE;
					 } 
				  }else if (pvalue >= 0.95){
					 if (pvalue >= (1.0 - 0.0001)){
					    dec <- '*** hypothesis accepted';
		                rejected <- FALSE;
					 }else if (pvalue >= (1.0 - 0.001)){
					    dec <- '** hypothesis accepted';
		                 rejected <- FALSE;
					 }else{
					    dec <- '* hypothesis accepted';
		                rejected <- FALSE;
					 } 				  	
			      }else{
			         dec <- 'hypothesis INCONCLUSIVE';
	                         rejected <- FALSE;
			      }
			      test <- "Welch T-test";
			   }else{ # Dependientes
			      cat("Dependientes\n");
		   	      r <- t.test(sample_1,sample_2,paired=TRUE,alternative=type_comparison);		      
			      pvalue <- r$p.value;
			      if (pvalue <= 0.05){
					 if (pvalue < 0.0001){
					    dec <- '*** hypothesis rejected';
		                            rejected <- TRUE;
					 }else if (pvalue < 0.001){
					    dec <- '** hypothesis rejected';
		                            rejected <- TRUE;
					 }else{
					    dec <- '* hypothesis rejected';
		                            rejected <- TRUE;
					 } 
				  }else if (pvalue >= 0.95){
					 if (pvalue >= (1.0 - 0.0001)){
					    dec <- '*** hypothesis accepted';
		                rejected <- FALSE;
					 }else if (pvalue >= (1.0 - 0.001)){
					    dec <- '** hypothesis accepted';
		                 rejected <- FALSE;
					 }else{
					    dec <- '* hypothesis accepted';
		                rejected <- FALSE;
					 } 				  	
			      }else{
			         dec <- 'hypothesis INCONCLUSIVE';
	                         rejected <- FALSE;
			      }
			      if (type_comparison == 'two.sided'){
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'and subset',lista_subsets[[2]] ,'come from SAME Distribution.');
			      	 }else{ 
			      	 	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'and sample',lista_variables[[2]],'of subset',lista_subsets[[2]] ,'come from SAME Distribution.');
			      	 }
			      }else if (type_comparison == 'greater'){
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'IS GREATER THAN subset',lista_subsets[[2]]);
			      	 }else{ 
				      	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'IS GREATER THAN sample',lista_variables[[2]],'of subset',lista_subsets[[2]],'.');
			      	 }
			      	 # desc <- 'The hypothesis evaluated in the test affirms that the first sample is greater than the second sample.'
			      }else{
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'IS LOWER THAN subset',lista_subsets[[2]]);
			      	 }else{ 
				      	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'IS LOWER THAN sample',lista_variables[[2]],'of subset',lista_subsets[[2]],'.');
			      	 }
			      	 # desc <- 'The hypothesis evaluated in the test affirms that the first sample is lower than the second sample.'
			      }
			      test <- "Two Paired T-test";
		      	   }
		   }else{ # No parametrico, funciona para independientes y para dependientes
	     	      cat("No siguen dist. En este caso funciona igual para dependientes e idependientes\n");
		   	  	  r <- ks.test(sample_1,sample_2,alternative=type_comparison); 
			  	  pvalue <- r$p.value;
			      if (pvalue <= 0.05){
					 if (pvalue < 0.0001){
					    dec <- '*** hypothesis rejected';
		                            rejected <- TRUE;
					 }else if (pvalue < 0.001){
					    dec <- '** hypothesis rejected';
		                            rejected <- TRUE;
					 }else{
					    dec <- '* hypothesis rejected';
		                            rejected <- TRUE;
					 } 
				  }else if (pvalue >= 0.95){
					 if (pvalue >= (1.0 - 0.0001)){
					    dec <- '*** hypothesis accepted';
		                rejected <- FALSE;
					 }else if (pvalue >= (1.0 - 0.001)){
					    dec <- '** hypothesis accepted';
		                 rejected <- FALSE;
					 }else{
					    dec <- '* hypothesis accepted';
		                rejected <- FALSE;
					 } 				  	
			      }else{
			         dec <- 'hypothesis INCONCLUSIVE';
	                         rejected <- FALSE;
			      }
			      if (type_comparison == 'two.sided'){
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'and subset',lista_subsets[[2]] ,'come from SAME Distribution.');
			      	 }else{ 
			      	 	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'and sample',lista_variables[[2]],'of subset',lista_subsets[[2]] ,'come from SAME Distribution.');
			      	 }
			      }else if (type_comparison == 'greater'){
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'IS GREATER THAN subset',lista_subsets[[2]]);
			      	 }else{ 
				      	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'IS GREATER THAN sample',lista_variables[[2]],'of subset',lista_subsets[[2]],'.');
			      	 }
			      	 # desc <- 'The hypothesis evaluated in the test affirms that the first sample is greater than the second sample.'
			      }else{
			      	 if (lista_variables[[1]] == lista_variables[[2]]){
			      	 	desc <- paste('Comparing ',lista_variables[[1]],', subset',lista_subsets[[1]],'IS LOWER THAN subset',lista_subsets[[2]]);
			      	 }else{ 
				      	desc <- paste('Sample',lista_variables[[1]],'of subset',lista_subsets[[1]],'IS LOWER THAN sample',lista_variables[[2]],'of subset',lista_subsets[[2]],'.');
			      	 }
			      	 # desc <- 'The hypothesis evaluated in the test affirms that the first sample is lower than the second sample.'
			      }
			   test <- "Two paired Kolmogorov Smirnov test";
		   	   }
	        }else if (length(lista_samples) > 2){ # Mas de dos muestras
		   cat("Numero de samples:",length(lista_samples),"\n");
		   decision <- TRUE;
		   tamMaximo <- 1000000;
		   for (i in 1:length(lista_samples)){
		       if (sigueNormal(lista_samples[[i]]) == FALSE){
		       	  	decision <- FALSE;
		       }
		       if (tamMaximo > length(lista_samples[[i]]))
		 	   tamMaximo <- length(lista_samples[[i]]);
		   }
		    info <- paste('Selecting',tamMaximo,'values as population samples for comparison of each variable.');
		    cat('Selecting',tamMaximo,'values as population samples for comparison of each variable.');
			if (tamMaximo < 50){
		   		warn <- "Length of samples should be greater than 50 in order to correctly compare.";
		    }
		    if (decision == TRUE){ # Parametrico
	     	         cat("Siguen una dist. Normal\n");
		         m <- matrix(0,ncol=length(lista_samples),nrow=length(lista_samples[[1]]));
			 dimnames(m) <- list(NULL,c(1:length(lista_samples)));
			 m <- data.frame(m);
			 for (i in 1:length(lista_samples)){
			     m[,i] <- sample(lista_samples[[i]],size=tamMaximo,replace=FALSE);
			 }
			 #print(m);
		      if (type_data == "i"){ # independiente (one-way ANOVA)
		         cat("Independientes\n");
		      	 exps <- stack(m);
			 print(exps);
			 r <- anova(lm(values~ind,data=exps));
			 pvalue <- summary(r);
	  	         test <- "Anova";
		      }else{ # dependientes (repeated measures ANOVA)
		         cat("Dependientes\n");
		      	 exps <- stack(m);
			 r <- aov(lm(values~ind,data=exps));
			 pvalue <- summary(r);
	  	         test <- "Repeated measures Anova";
		      }
		   }else{ # No parametrico
		      cat("No siguen una distribucion.\n");
		      if (type_data == "i"){ # independiente (Kruskal Wallis)
		         cat("Independientes\n");
			   	for (i in 1:length(lista_samples)){
			   		lista_samples[[i]] <- sample(lista_samples[[i]],size=tamMaximo,replace=FALSE);
				}		         
		      	 r <- kruskal.test(lista_samples);
			 	 pvalue <- r$p.value;
			      if (pvalue <= 0.05){
					 if (pvalue < 0.0001){
					    dec <- '*** hypothesis rejected';
		                            rejected <- TRUE;
					 }else if (pvalue < 0.001){
					    dec <- '** hypothesis rejected';
		                            rejected <- TRUE;
					 }else{
					    dec <- '* hypothesis rejected';
		                            rejected <- TRUE;
					 } 
				  }else if (pvalue >= 0.95){
					 if (pvalue >= (1.0 - 0.0001)){
					    dec <- '*** hypothesis accepted';
		                rejected <- FALSE;
					 }else if (pvalue >= (1.0 - 0.001)){
					    dec <- '** hypothesis accepted';
		                 rejected <- FALSE;
					 }else{
					    dec <- '* hypothesis accepted';
		                rejected <- FALSE;
					 } 				  	
			      }else{
			         dec <- 'hypothesis INCONCLUSIVE';
	                         rejected <- FALSE;
			      }
		      	  desc <- 'The hypothesis evaluated in the test affirms that ALL the variables included in the Statistical Analysis COME FROM THE SAME Distribution.'
	  	          test <- "Kruskal Wallis test";
		      }else{ # dependientes (Friedman)
		         cat("Dependientes\n");
		         m <- matrix(0,ncol=length(lista_samples),nrow=length(lista_samples[[1]]));
			 dimnames(m) <- list(NULL,c(1:length(lista_samples)));
			 for (i in 1:length(lista_samples)){
			     m[,i] <- sample(lista_samples[[i]],size=tamMaximo,replace=FALSE);
			 }
		      	 r <- friedman.test(m);
			 pvalue <- r$p.value;
			      if (pvalue <= 0.05){
					 if (pvalue < 0.0001){
					    dec <- '*** hypothesis rejected';
		                            rejected <- TRUE;
					 }else if (pvalue < 0.001){
					    dec <- '** hypothesis rejected';
		                            rejected <- TRUE;
					 }else{
					    dec <- '* hypothesis rejected';
		                            rejected <- TRUE;
					 } 
				  }else if (pvalue >= 0.95){
					 if (pvalue >= (1.0 - 0.0001)){
					    dec <- '*** hypothesis accepted';
		                rejected <- FALSE;
					 }else if (pvalue >= (1.0 - 0.001)){
					    dec <- '** hypothesis accepted';
		                 rejected <- FALSE;
					 }else{
					    dec <- '* hypothesis accepted';
		                rejected <- FALSE;
					 } 				  	
			      }else{
			         dec <- 'hypothesis INCONCLUSIVE';
	                         rejected <- FALSE;
			      }
		      	  desc <- 'The hypothesis evaluated in the test affirms that ALL the variables included in the Statistical Analysis COME FROM THE SAME Distribution.'
	  	         test <- "Friedman test";
		      }
		   }
		}
		return(list(pvalue=pvalue,decision=dec,test=test,desc=desc,rejected=rejected,warning=warn,info=info));
	}


	# Analiza si una muestra sigue una distribucion normal o no
	sigueNormal <- function(list){
		if (length(list) < 10) return(FALSE);
		f <- fitdist(list,"norm");
		stat <- gofstat(f);
		if (stat$kstest == "not rejected"){
		   return(TRUE);
		}else{
		   return(FALSE);
		}
	}


	# Compara dos muestras a traves de wilcox y ks
	#compareTwo <- function(list){
	#        type_comparison <- "two.sided";
	#	type_data <- "c";
	#	if (length(list) < 2){ 
	#	  return(-1);
	#	}else if (length(list) > 2){
	#	  if (list[[3]] != "c" && list[[3]] != "d") return(-1);
	#	  type_data <- list[[3]];
	#	  if (length(list) >= 4){
	#	     if (list[[4]] != "two.sided" && list[[4]] != "greater" && list[[4]] != "less") return(-1);
	#	     type_comparison <- list[[4]];
	#          }
	#	}
	#
	#	
	#	if (type_data=="c"){
	#		# usamos ks y mann-whitney
	#		wr <- wilcox.test(list1,list2,type_comparison);
	#		ksr <- ks.test(list1,list2, alternative=type_comparison);
	#		return(list(wilcox=wr$p.value,ks=ksr$p));
	#	}else if (type_data=="d"){
	#		# usamos wilcox para datos categoricos.
	#		wr <- wilcox.test(list1,list2,type_comparison);
	#		return(list(wilcox=wr$p.value,ks=0.0));		
	#	}
	#}


	# Devuelve los estadisticos basicos de un conjunto de datos (media, mediana, desv, quartiles, etc.)
	basicStats <- function(list){
		if (length(list) != 1) return(-1);
		lista <- list[[1]];
		res <- summary(lista);
		res2 <- cbind(res);
		#print(res);
		return(list(min=res2[1],q1=res2[2],median=res2[3],mean=res2[4],q3=res2[5],max=res2[6],sd=sd(lista)))
	}



	# Se encarga de indicar que distribuciones se ajustan o no a un conjunto de valores (continuos o discretos), basandose en el test KS
	distributionOf <- function(list){
		type_dist <- "c";
		if (length(list) < 1) return(-1);
		lista <- list[[1]];
		if (length(list) >= 2){
	          if (list[[2]] != "c" && list[[2]] != "d") return(-1);
		  type_dist <- list[[2]];
		}
		res <- list();
		idx <- 1;
		if (type_dist == "c"){
			distr <- c("norm","lnorm","gamma","exp","weibull");
		}else if (type_dist == "d"){
			distr <- c("binom","nbinom","geom","hyper","pois");
		}else{
			distr <- c();
		}
		for (i in 1:length(distr)){
			#cat("distribution: ",distr[i],"\n");
			f <- fitdist(lista,distr[i]);
			stat <- gofstat(f);
			if (stat$kstest == "not rejected"){
				res[[idx]] = distr[i];
				idx <- idx + 1;
			}
		}
		return(list(dist=res));
	}


	# Obtiene toda la informacion obtenida al ajustar una distribucion sobre un conjunto de valores
	getInfoDistribution <- function(list){
	     if (length(list) == 2){
	        lista <- list[[1]];
		if (list[[2]] != "norm" && list[[2]] != "lnorm" && list[[2]] != "gamma" && list[[2]] != "exp" && list[[2]] != "weibull" && list[[2]] != "binom" && list[[2]] != "nbinom" && list[[2]] != "geom" && list[[2]] != "hyper" && list[[2]] != "pois") return(-1);
		dist <- list[[2]];
		f <- fitdist(lista,dist);
		stat <- gofstat(f);
		svg(filename="/tmp/rplot.svg");
		plot(f);
		dev.off();
		imagen <- "";
		fichero <- file('/tmp/rplot.svg','r');
		while (length(input <- readLines(fichero,n=1000)) > 0){
			for (i in 1:length(input)){
				imagen <- paste(imagen,input[i],sep="\n");
			}
		}
		return(list(chisq=stat$chisqpvalue,cramer.value=stat$cvm,cramer.test=stat$cvmtest,ad.value=stat$ad,ad.test=stat$adtest,ks.value=stat$ks,ks.test=stat$kstest,estimate=f$estimate,plot=imagen));
	     }else{
	       return(-1);
	     }
	}



	#===============================================================================
	#   Main Method
	#===============================================================================

	# Almacenamos en una lista los servicios disponibles de Stats
	services <- c("compareAll","compare","statSort","correlation","basicStats","distributionOf","getInfoDistribution");

	# Leemos los argumentos de entrada, que son el puerto de entrada y de salida
	args <- commandArgs(trailingOnly = TRUE)
	msg_id <- 1
	service_port <- args[1];
	control_port <- args[2];

	context = init.context();

	# Creamos el socket del servicio Stats
	service_socket = init.socket(context,"ZMQ_REP");
	bind.socket(service_socket,paste("tcp://127.0.0.1:",service_port,sep=""));

	# Nos conectamos al socket control del Core Central
	control_socket = init.socket(context,"ZMQ_REQ");
	connect.socket(control_socket,paste("tcp://127.0.0.1:",control_port,sep=""));

	# Mandamos los servicios que disponemos en Stats a traves del socket control
	params <- list(service_name='StatsSrv',endpoint=paste("tcp://127.0.0.1:",service_port,sep=""),srv_description=srv_description());
	mensaje <- JSONRPC.Request('FrontSrv.expose',params,msg_id);
	#cat(mensaje,'\n');
	send.raw.string(control_socket, mensaje);
	receive.string(control_socket); # Recibo el ACK

	while (1){
	        cat("Recieving request...\n");
		msg = receive.string(service_socket);
		cat(msg,"\n");
		tryCatch({
			msg <- fromJSON(msg)
			cat("Calling",msg$method,"...\n");
			if (!is.element(msg$method,services)){
			   mensaje <- JSONRPC.ErrorResponse("JSONRPC.MethodNotFoundError",list(),msg_id);
		           send.raw.string(service_socket,mensaje);
			}else{
			   result <- do.call(msg$method,list(list=msg$params));
			   cat (JSONRPC.SuccessResponse(result,msg$id));
			   if (is.numeric(result) && result == -1){
		 	      mensaje <- JSONRPC.ErrorResponse("JSONRPC.InvalidParamsError",list(),msg_id);
		              send.raw.string(service_socket,mensaje);
			   }else{
			      mensaje <- JSONRPC.SuccessResponse(result,msg$id);
			      cat("Sending result...\n");
			      #cat(mensaje,"\n");
			      send.raw.string(service_socket,mensaje);
			   }
			}
	        },error = function(err){
	           message ("********* ERROR: ");
	           message (err);
	           message (" \n");
		   mensaje <- JSONRPC.ErrorResponse("JSONRPC.InvalidRequestError",list(),msg_id);
		   send.raw.string(service_socket,mensaje);
	        },finally = function(w){});
	}