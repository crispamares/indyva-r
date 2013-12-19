#install.packages(c("rzmq","fitdistrplus","rjson"));

library("rzmq");
library("rjson");
library('fitdistrplus');


# Se encarga de indicar que distribuciones se ajustan o no a un conjunto de valores (continuos o discretos), basandose en el test KS
distributionOf <- function(lista,type="continuous"){
	res <- c();
	if (type == "continuous"){
		distr <- c("norm","lnorm","gamma","exp","weibull");
	}else if (type == "categorical"){
		distr <- c("binom","nbinom","geom","hyper","pois");
	}else{
		distr <- c();
	}
	for (i in 1:length(distr)){
		#cat("distribution: ",distr[i],"\n");
		f <- fitdist(lista,distr[i]);
		stat <- gofstat(f);
		if (stat$kstest == "not rejected"){
			res <- c(res,distr[i]);
		}
	}
	return(res);
}

# Obtiene toda la información obtenida al ajustar una distribucion sobre un conjunto de valores
getInfoDistribution <- function(lista,dist){
	f <- fitdist(lista,dist);
	stat <- gofstat(f);
	return(stat);
}


# Compara dos muestras a traves de wilcox y ks
compareTwo <- function(lista1,lista2,type="continuous",type_comparison="two.sided"){
	if (type=="continuous"){
		# usamos ks y mann-whitney
		wr <- wilcox.test(lista1,lista2,type_comparison);
		ksr <- ks.test(lista1,lista2,type_comparison);
		return(list(wr$p,ksr$p));
	}else if (type=="categorical"){
		# usamos wilcox para datos categoricos.
		wr <- wilcox.test(lista1,lista2);
		return(wr$p);		
	}
}


# Compara N muestras a traves de kruskal wallis
compareNSamples <- function(listado,func=""){
	if (func == ""){
		res <- kruskal.test(listado);
	}else{
		res <- kruskal.test(func,data=listado);
	}
	return(res$p);
}


# Realiza un resumen de media, mediana, desv, mad, y otros estadisticos de una lista o de un conjunto de listas.
resume <- function(lista){
	return(summary(lista));
}


# Funcion MAIN que se mantiene a la escucha de solicitudes por puerto 5557 y devuelve resultados por 5558
context = init.context();
in.socket = init.socket(context,"ZMQ_PULL");
bind.socket(in.socket,"tcp://*:5557");

out.socket = init.socket(context,"ZMQ_PUSH");
bind.socket(out.socket,"tcp://*:5558");


while (1){
	msg = receive.socket(in.socket)
	msg <- fromJSON(msg);
	fun <- msg$fun;
	args <- msg$args;
	result <- do.call(fun,args);
	result <- toJSON(result);
	send.socket(out.socket,result);
}
