ds.mean.pb<-function(x=NULL, type='split', datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }
  
  if(is.null(x)){
    stop("Please provide the name of the input vector!", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  varname <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  if(is.na(obj2lookfor)){
    defined <- isDefined(datasources, varname)
  }else{
    defined <- isDefined(datasources, obj2lookfor)
  }
  
  # call the internal function that checks the input object is of the same class in all studies.
  typ <- checkClass(datasources, x)
  
  # the input object must be a numeric or an integer vector
  if(typ != 'integer' & typ != 'numeric' & typ != 'factor'){
    stop("The input object must be an integer or a numeric vector.", call.=FALSE)
  }
  
  # number of studies
  num.sources <- length(datasources)
  
  cally <- paste0("meanDS.pb(", x, ")")
  ss.obj <- datashield.aggregate(datasources, as.symbol(cally))
  Nstudies<-length(datasources)
  ss.mat<-t(matrix(unlist(ss.obj),ncol=Nstudies))
  dimnames(ss.mat)<-list(names(ss.obj),names(ss.obj[[1]]))


  if (type=='split'|type=="s") {
  	return(ss.mat)
  	}

  if (type=="combined"|type=="combine"|type=="c") {
	ss.mat.combined<-t(matrix(ss.mat[1,]))
	dimnames(ss.mat.combined)<-list("studiesCombined",names(ss.obj[[1]]))
	ss.mat.combined[1,1]<-(t(matrix(ss.mat[,3]))%*%ss.mat[,1])/sum(ss.mat[,3])
	ss.mat.combined[1,2]<-sum(ss.mat[,2])
	ss.mat.combined[1,3]<-sum(ss.mat[,3])
	ss.mat.combined[1,4]<-sum(ss.mat[,4])

	return(ss.mat.combined)
	}

  if (type=="both"|type=="b") {
	ss.mat.combined<-t(matrix(ss.mat[1,]))
	dimnames(ss.mat.combined)<-list("studiesCombined",names(ss.obj[[1]]))
	ss.mat.combined[1,1]<-(t(matrix(ss.mat[,3]))%*%ss.mat[,1])/sum(ss.mat[,3])
	ss.mat.combined[1,2]<-sum(ss.mat[,2])
	ss.mat.combined[1,3]<-sum(ss.mat[,3])
	ss.mat.combined[1,4]<-sum(ss.mat[,4])
	
	ss.mat.both<-rbind(ss.mat,ss.mat.combined)
	return(ss.mat.both)
	}


}

#<environment: namespace:dsBaseClient>