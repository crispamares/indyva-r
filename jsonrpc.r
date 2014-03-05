# jsonrpc.R -                                                                                                     
# Created on 18/12/2013     
# Modified on 05/03/2014
# @author: sgonzalez                                                                                                 

                                                                                                                    
library("rjson");


JSONRPC.Protocol <- function(){
   return("2.0");
}

JSONRPC.SuccessResponse <- function(m,p,id=1){
    msg <- list(jsonrpc=JSONRPC.Protocol(),id=id,method=m,params=p);
    return(toJSON(msg));
}


JSONRPC.SuccessResponse <- function(r,id=1){
    msg <- list(jsonrpc=JSONRPC.Protocol(),id=id,result=r);
    return(toJSON(msg));
}


JSONRPC.InvalidRequestError <- function(...){
  return(list(code=-32600,message="Invalid Request"));
}


JSONRPC.MethodNotFoundError <- function(...){
  return(list(code=-32601,message="Method not found"));
}


JSONRPC.InvalidParamsError <- function(...){
  return(list(code=-32602,message="Invalid Params"));
}


JSONRPC.InternalError <- function(...){
  return(list(code=-32603,message="Internal Error"));
}


JSONRPC.ServerError <- function(...){
  return(list(code=-32000,message=""));
}


JSONRPC.SpecificError <- function(info){
  return(list(code=info.code,message=info.message));
}



JSONRPC.ErrorResponse <- function(f,info,id=1){
   datos <- do.call(f,info);
   msg <- list(jsonrpc=JSONRPC.Protocol(),id=id,error=datos);
   return(toJSON(msg));
}


JSONRPC.Request <- function(m,p,id=1){
   msg <- list(jsonrpc=JSONRPC.Protocol(),id=id,method=m,params=p);
   return(toJSON(msg));
}