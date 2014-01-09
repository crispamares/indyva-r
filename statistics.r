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

# genera el paquete de emision JSONRPC con el metodo y los parámetros
pack_msg <- function(m,p,id=1){
    msg <- list(jsonrpc="2.0",id=id,method=m,params=p);
	return(toJSON(msg));
}


#===============================================================================
#   Public methods
#===============================================================================


srv_description <- function(){
	cat('srv_description\n');
	fn <- list(
			compareTwo = list(
					params = list(list1="double array, first list of values",list2="double array, second list of values",type="char, representing continuous data (c) or discrete data (d)",type_comparison="string, possible values: two.sided | greater | less"),
					returned = list(ks="double, p value of the KS test", wilcox="double, p value of the Wilcoxon test")
			),
			basicStats = list(
					params = list(list="double array, list of values"),
					returned = list(min="double, min value of the list",q1="double, first quartile value of the list",median="double, median value of the list",mean="double, mean value of the list",q3="double, third quartile value of the list",max="double, max value of the list",sd="double, std. deviation value of the list")
			),
			distributionOf = list(
					params = list(list="double array, list of values",type="char, representing continuous data (c) or discrete data (d)"),
					returned = list(dist="list of distibutions not rejected")
			),
			getInfoDistribution = list(
					params = list(list="double array, list of values",dist="string, representing the continuous or discrete distribution"),
					returned = list(chisq="double, p value of the ChiSq test",cramer="double, p value of the Von Cramer test",ad="double, p value of the Anderson Darling test",ks="double, p value of the KS test",estimate="list of doubles, parameters estimation of the distribution",plot="svg image, plots of fitting distribution")
			)
		);
}



# Compara dos muestras a traves de wilcox y ks
compareTwo <- function(list1,list2,type="c",type_comparison="two.sided"){
	if (type=="c"){
		# usamos ks y mann-whitney
		wr <- wilcox.test(list1,list2,type_comparison);
		ksr <- ks.test(list1,list2,type_comparison);
		return(list(wilcox=wr$p,ks=ksr$p));
	}else if (type=="d"){
		# usamos wilcox para datos categoricos.
		wr <- wilcox.test(list1,list2,type_comparison);
		return(list(wilcox=wr$p,ks=0.0));		
	}
}


# Devuelve los estadisticos basicos de un conjunto de datos (media, mediana, desv, quartiles, etc.)
basicStats <- function(list){
	res <- summary(list);
	return(list(min=res["Min."],q1=res["1st Qu."],median=res["Median"],mean=res["Mean"],q3=res["3rd Qu."],max=res["Max."],sd=sd(list)))
}



# Se encarga de indicar que distribuciones se ajustan o no a un conjunto de valores (continuos o discretos), basandose en el test KS
distributionOf <- function(list,type="c"){
	res <- c();
	if (type == "c"){
		distr <- c("norm","lnorm","gamma","exp","weibull");
	}else if (type == "d"){
		distr <- c("binom","nbinom","geom","hyper","pois");
	}else{
		distr <- c();
	}
	for (i in 1:length(distr)){
		#cat("distribution: ",distr[i],"\n");
		f <- fitdist(list,distr[i]);
		stat <- gofstat(f);
		if (stat$kstest == "not rejected"){
			res <- c(res,distr[i]);
		}
	}
	return(list(dist=res));
}


# Obtiene toda la información obtenida al ajustar una distribucion sobre un conjunto de valores
getInfoDistribution <- function(lista,dist){
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
	return(list(chisq=stat$chisqpvalue,cramer=stat$cvmtest,ad=stat$adtest,ks=stat$kstest,estimate=f$estimate,plot=imagen));
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


mensaje <- pack_msg('FrontSrv.expose',c('StatsSrv', paste("tcp://*:",in_port,sep=""), srv_description()));
cat(mensaje,'\n');
send.raw.string(out.socket, mensaje);
receive.string(out.socket);

while (1){
	msg = receive.string(in.socket)
	msg <- fromJSON(msg);
	method <- msg$method;
	params <- msg$params;
	id <- msg$id;
	result <- do.call(method,params);
	mensaje <- pack_msg(method,result,id);
	send.raw.string(out.socket,mensaje);
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

