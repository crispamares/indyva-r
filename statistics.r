# statistics.r - 
# Created on 18/12/2013
# @author: sgonzalez

# TODO: Detectar excepciones y tratarlos
# TODO: programar Kruskal-wallis


# En caso de ser necesario, instalar las librerias, copiando la siguiente linea.
# install.packages(c("rzmq","fitdistrplus","rjson"));

library("rzmq");
library("rjson");
library('fitdistrplus');

if(!exists("JSONRPC.Protocol", mode="function")) source("jsonrpc.r");


#===============================================================================
#   Public methods
#===============================================================================


srv_description <- function(){
    cat('srv_description\n');
	fn <- list(
			compareTwo = list(
					params = list( c("list1", "double array, first list of values"),
					       	       c("list2", "double array, second list of values"),
						       c("type", "char, representing continuous data (c) or discrete data (d), default=c"),
						       c("type_comparison", "string, possible values: two.sided | greater | less, default=two.sided")
						       ),
					return = list(ks="double, p value of the KS test", 
					       	      wilcox="double, p value of the Wilcoxon test")
			),
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


# Compara dos muestras a traves de wilcox y ks
compareTwo <- function(list){
        type_comparison <- "two.sided";
	type_data <- "c";
	if (length(list) < 2){ 
	  return(-1);
	}else if (length(list) > 2){
	  if (list[[3]] != "c" && list[[3]] != "d") return(-1);
	  type_data <- list[[3]];
	  if (length(list) >= 4){
	     if (list[[4]] != "two.sided" && list[[4]] != "greater" && list[[4]] != "less") return(-1);
	     type_comparison <- list[[4]];
          }
	}else if (length(list) == 2){
           list1 <- list[[1]];
	   list2 <- list[[2]];
	}
	if (type_data=="c"){
		# usamos ks y mann-whitney
		wr <- wilcox.test(list1,list2,type_comparison);
		ksr <- ks.test(list1,list2,type_comparison);
		return(list(wilcox=wr$p.value,ks=ksr$p));
	}else if (type_data=="d"){
		# usamos wilcox para datos categoricos.
		wr <- wilcox.test(list1,list2,type_comparison);
		return(list(wilcox=wr$p.value,ks=0.0));		
	}
}


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
services <- c("compareTwo","basicStats","distributionOf","getInfoDistribution");

# Leemos los argumentos de entrada, que son el puerto de entrada y de salida
args <- commandArgs(trailingOnly = TRUE)
msg_id <- 1
service_port <- args[1];
control_port <- args[2];

context = init.context();

# Creamos el socket del servicio Stats
service_socket = init.socket(context,"ZMQ_REP");
bind.socket(service_socket,paste("tcp://*:",service_port,sep=""));

# Nos conectamos al socket control del Core Central
control_socket = init.socket(context,"ZMQ_REQ");
connect.socket(control_socket,paste("tcp://*:",control_port,sep=""));

# Mandamos los servicios que disponemos en Stats a traves del socket control
params <- list(service_name='StatsSrv',endpoint=paste("tcp://*:",service_port,sep=""),srv_description=srv_description());
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
	   mensaje <- JSONRPC.ErrorResponse("JSONRPC.InvalidRequestError",list(),msg_id);
	   send.raw.string(service_socket,mensaje);
        },finally = function(w){});
}

#######################################################################################################################################

# Compara N muestras a traves de kruskal wallis
#compareNSamples <- function(listado,func=""){
#	if (func == ""){
#		res <- kruskal.test(listado);
#	}else{
#		res <- kruskal.test(func,data=listado);
#	}
#	return(res$p);
#}
