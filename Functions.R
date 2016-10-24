###########################################################################################
###########################################################################################
CA <- function (x0, mask, Suit, Acc, Nhood, Demand, Random, Inertia
                ){
	TP <- (Acc)*(Suit+1)*(Nhood+1)*(Random) * Inertia
    TP = mask(TP, mask)
    ord = sort(as.numeric(as.matrix(TP)), decreasing=TRUE);
    keyvalue = ord[round(Demand)]
    xnew = mask*0
    xnew[TP>=keyvalue] =1
    return(xnew)
}

###########################################################################################
###########################################################################################
fun.dist <- function (nr,nc=nr, method=2){
    dr = 1:nc
    for(i in 2:nc){
        dr=rbind(dr, 1:nc)
    }
       dc = 1:nc
    for(i in 2:nr){
        dc=cbind(dc, 1:nr)
    } 
   
    dr = abs(dr - ceiling(nc/2))
    dc = abs(dc - ceiling(nr/2) )
    if(method ==1){
        D = sqrt( dr^2 + dc^2)  # Euclidean
    }else{  
        D = dr + dc;        # Manhattan
    }
    return(D)
}



###########################################################################################
###########################################################################################
rsmp <- function(x, mask){ 
    return( mask(resample(x, mask),mask) )
}

###########################################################################################
###########################################################################################
UrbanCenter <- function(x, n=11, w, threshold, nonUrban=NA, urbanvalue=1, if.save=TRUE){
    if(missing(w)){
        w = fun.dist(n, method=2)
        w[ w<n/2 ]=1
        w[ w>n/2 ]= 0;
        message('Urban Center weight = ')
        print(w)
    }
    nb <- focal(x, w=w)
    
    if(missing(threshold)){
        trd =cellStats(nb, max)*0.8
    }else{
        trd=threshold
    }
    
    nbo=nb;
    nbo[nbo<=trd]=nonUrban
    if( urbanvalue >0){
        nbo[nbo>trd]=1 
    }
    if(if.save){
       fn= 'Data/UrbanCenter.asc';
       message('\tWriting file', fn)
       writeRaster(filename=fn, nbo, overwrite=TRUE)   
    }
    return(nbo)
}

###########################################################################################
###########################################################################################
AccFunc <- function(x, mask, dc){
    acc = (1 + x0/dc) ^ (-1)
    return(acc)
}

###########################################################################################
###########################################################################################
RandFunc <- function(mask, alpha=0.5){
    alpha=min(alpha, 1)
    alpha = max(1e-3, alpha)
    
    DIM= dim(mask)
    nr = DIM[1]
    nc = DIM[2]
    #----------------------
    #Random.
	x.rand <-matrix(runif(prod(dim(mask)) ), nrow = nr, ncol=nc)
	w1 <- ( -log(1 - x.rand  ) )^alpha
    rnd = mask;
    values(rnd)=w1;
    ret = rsmp(rnd, mask)
    mx = cellStats(ret, max)
    ret = ret/mx;
    return(ret)
}

###########################################################################################
###########################################################################################
NhoodFunc <- function(x, mask, w){ 
    if(missing(w)){
        w <- matrix(c(0,0,50,0,0,
                      0,50,150,50,0,
                      50,150,500,150,50,
                      0,50,150,50,0,
                      0,0,50,0,0), nr=5,nc=5)	
    }
	nb <- focal(x, w=w)
    ret = rsmp(nb, mask)
    mx = cellStats(ret, max)
    ret = ret/mx;
    return(ret)
}
###########################################################################################
###########################################################################################
InertiaFunc <- function(
    intertia = c(11, 1,
                 21, 0,
                 22,0.5,
                 23, 0.8,
                 24, 1)
                        ){
    
}
###########################################################################################
###########################################################################################
NLCD.reclassify <- function(x, key, v.key=1, v.rest=0){
    x=round(x);
    ulc = unique(x)
    rcl = cbind(ulc, v.rest)
    rcl[ulc %in% key, 2]=v.key;
    ret = reclassify(x,rcl)
    return(ret)
}
