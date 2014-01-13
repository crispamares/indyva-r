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

#===============================================================================
#   Auxiliar function
#===============================================================================

# genera el paquete de emision JSONRPC con el metodo y los parÃ¡metros
pack_msg <- function(m,p,id=1){
    msg <- list(jsonrpc="2.0",id=id,method=m,params=p);
    return(toJSON(msg));
}


#===============================================================================
#   Public methods
#===============================================================================


StatsSrv.srv_description <- function(){
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
StatsSrv.compareTwo <- function(list){
        type_comparison <- "two.sided";
	type_data <- "c";
	if (length(list) > 2){
	  type_data <- list[[3]];
	  if (length(list) >= 4){
	     type_comparison <- list[[4]];
          }
	}else if (length(list) == 2){
           list1 <- list[[1]];
	   list2 <- list[[2]];
	}else{
	   cat("ERROR EN EL NUMERO DE ARGUMENTOS!\n");
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
StatsSrv.basicStats <- function(list){
	lista <- list[[1]];
	res <- summary(lista);
	res2 <- cbind(res);
	#print(res);
	return(list(min=res2[1],q1=res2[2],median=res2[3],mean=res2[4],q3=res2[5],max=res2[6],sd=sd(lista)))
}



# Se encarga de indicar que distribuciones se ajustan o no a un conjunto de valores (continuos o discretos), basandose en el test KS
StatsSrv.distributionOf <- function(list){
	type_dist <- "c";
	lista <- list[[1]];
	if (length(list) >= 2){
	  type <- list[[2]];
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
StatsSrv.getInfoDistribution <- function(list){
     if (length(list) == 2){
        lista <- list[[1]];
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
       cat("ERROR EN LOS PARAMETROS!\n");
     }
}



#===============================================================================
#   Main Method
#===============================================================================

# Leemos los argumentos de entrada, que son el puerto de entrada y de salida
args <- commandArgs(trailingOnly = TRUE)
msg_id <- 1
in_port <- args[1];
out_port <- args[2];

context = init.context();
in.socket = init.socket(context,"ZMQ_REP");
bind.socket(in.socket,paste("tcp://*:",in_port,sep=""));

out.socket = init.socket(context,"ZMQ_REQ");
connect.socket(out.socket,paste("tcp://*:",out_port,sep=""));


mensaje <- pack_msg('FrontSrv.expose',list(service_name='StatsSrv', 
					   endpoint=paste("tcp://*:",in_port,sep=""), 
					   srv_description=StatsSrv.srv_description()));
cat(mensaje,'\n');
send.raw.string(out.socket, mensaje);
receive.string(out.socket); # Recibo el ACK

ack <- pack_msg('ack',list());

while (1){
	msg = receive.string(in.socket)
	msg <- fromJSON(msg);
	send.raw.string(in.socket, ack); # Envio el ACK
	cat("Calling",msg$method,"...\n");
	result <- do.call(msg$method,list(list=msg$params));
	mensaje <- pack_msg(msg$method,result,msg$id);
	send.raw.string(out.socket,mensaje);
	cat("Sending result...\n");
	receive.string(out.socket); # Recibo el ACK
	cat("Recieved ACK!\n");
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
