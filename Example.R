###########################################################################################
###########################################################################################
#LCM <- function(if.save=TRUE){
rm(list=ls());
require(raster)
require(maptools)
require(sp)
require(rgdal)
source('CTheta_func.R')
source('image.png.R')
source('NLCD.colors.R')

path = paste0('',strftime(Sys.time(), format='%Y%m%d_%H%M'))
ipng = TRUE
iasc = FALSE;
dyr = 10;
years=seq(2001, 2101, by=dyr)
nyr = length(years)

#Raster
    message('Initial Land use... ')
    LU=raster('LandCover2001.asc')
    LU0 = round(LU)
    image.png(x=LU0,fn='LU0', ipng=ipng,iasc=iasc, path=path, NLCD=TRUE)
    #mask
    #maskx = raster('Mask.tif')
    message('Mask ... ')
    fun <-function(x){x[ !is.na(x) ] = 1; return (x) }
    mask = calc(LU, fun)
    image.png(x=mask, fn='mask',ipng=ipng,iasc=iasc, path=path)
    #River
    message('River network... ')
    riv = raster('Data/riv.asc')
    riv[!is.na(riv)]=1
    riv = rsmp(riv, mask)
    image.png(x=riv, fn='riv',ipng=ipng,iasc=iasc, path=path)
    #River distance
#    message('Distance to River... ')
#    distriv = raster('dist_riv.asc')
#    distriv = rsmp(distriv, mask)
#    image.png(x=distriv, fn='distriv',ipng=ipng,iasc=iasc, path=path)
    #slope
#    slopex = raster('Data/slope.asc')
#    slopex = rsmp( slope, mask)
#    image.png(x=slopex, fn='slopex',ipng=ipng,iasc=iasc, path=path)
    #dem and slope
    message('DEM and slope... ')
    dem = raster('Data/dem_mask.tif')
    image.png(x=rsmp(dem,mask) , fn='dem',ipng=ipng,iasc=iasc, path=path)
    cor = raster(extent(dem), res=mean(res(dem))*10, crs=crs(dem))  #coarser DEM
    cor = resample(dem, cor)
    slope = terrain(cor, out='slope') * 100
    slope = rsmp(slope, mask)
    image.png(x=slope, fn='Slope',ipng=ipng,iasc=iasc, path=path)
    #Exclusive area
    message('Exclusive Area ... ')
     key = c(11)
     mask.wb = NLCD.reclassify(LU0, key=key, v.rest=NA)
    excl = cover(riv, mask.wb)
    excl[excl >0]=1 
    image.png(x=excl, fn='exclusive',ipng=ipng,iasc=iasc, path=path)
    
    #roads
    rd1 = rsmp(raster('Data/Roads.asc'), mask)
    rd2 = rsmp(raster('Data/StateRoads_Raster.tif'), mask)
    road = cover(rd1,rd2)
    road[road>0]=1
    image.png(x=road, fn='road',ipng=ipng,iasc=iasc, path=path)
    
    #dr = distance(road)
    #DR = mask(dr, mask)
    #writeRaster(x=DR, filename='Data/dist_road_all.asc', overwrite=TRUE)
    # mask0
    mask0 = mask
    mask0[ !is.na(excl) ]=NA
    image.png(x=mask0, fn='mask0',ipng=ipng,iasc=iasc, path=path)
    
    # Accessibility of road
    message('Accessibility to Roads and Urban Centers ... ')
    distroad = raster('Data/dist_road_all.asc')
    #distroad = raster('Data/dist_road_state.asc')
    dc = 500;
    acc.rd = (1 + distroad/dc) ^ (-1)
    # Accessibility of Urban
#    key.urban= c(23,24)
#    urban = NLCD.reclassify(LU, key.urban, v.rest=NA)
#    uc = urbanCenter(urban, n=11, if.save=if.save)
#    dist.urban = distance(uc)
#    du = rsmp(dist.urban, mask)
#    if(if.save){
#       fn= 'Data/distUrban.asc';
#       message('\tWriting file', fn)
#       writeRaster(filename=fn, du, overwrite=TRUE)   
#    }
    disturban = raster('Data/distUrban.asc')
    dc = 1000;
    acc.ub = (1 + disturban/dc) ^ (-1)

    gamma = 0.5
#    Acc=acc.ub * (1- gamma)+ acc.rd * gamma;
    Acc = acc.ub * acc.rd;
    image.png(x=distroad, fn='distroad',ipng=ipng,iasc=iasc, path=path)
    image.png(x=acc.rd, fn='acc.rd',ipng=ipng,iasc=iasc, path=path)
    image.png(x=disturban, fn='disturban',ipng=ipng,iasc=iasc, path=path)
    image.png(x=acc.ub, fn='acc.ub',ipng=ipng,iasc=iasc, path=path)
    image.png(x=Acc, fn='Acc',ipng=TRUE,iasc=iasc, path=path)
#  for (i in 0:10){
#        a = i/10;
#        image.png(x=acc.rd * a + acc.ub *(1-a) , fn=paste0('acc.rd.',i),ipng=TRUE,iasc=iasc, path=path)
#  }
    #Suitability
    message('Slope Suitability ... ')
    rcl.slope=t(matrix(c(-Inf,0.5,1,
                   0.5, 1,0.95,
                   1, 2,0.92,
                   2, 4,0.9,
                   4, 6,0.5,
                   6,10,0.4,
                   10,Inf,0.1),nrow=3))
    slope_suit <- reclassify(slope,rcl.slope)
    Suit = rsmp(slope_suit, mask0)
    image.png(x=Suit, fn='Slope_Suit',ipng=ipng,iasc=iasc, path=path)
    #
    y=LU
             source('CTheta_func.R')
             Demand=9000;    #number of cells
             key = c(21:24);# keys[[i]]
             x0 = NLCD.reclassify(y, key, v.rest=0)
             x0=mask(x0, mask0)
             image.png(x=x0,ipng=ipng,iasc=iasc, path=path)
             
            # uc=raster('Data/UrbanCenter.asc');
            # uc[uc>0]=1
            # uc[is.na(uc)]=0
            # x0=mask(uc,mask0)
source('C.R')

