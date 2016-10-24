image.png <- function(x, fn=deparse(substitute(x)), title=fn, path='./',
                      ht=1200, wd=1200, ipng = TRUE, iasc=TRUE, NLCD=FALSE, ... ){
    if(ipng){    
        path.png = file.path(path, 'png')
        dir.create(path.png, recursive=TRUE, showWarnings=FALSE)
        if(!grepl('.png$', fn)){
            filename=paste0(fn,'.png')
        }else{
            filename= fn;
        }
        png(filename=file.path(path.png, filename), height=ht, width=wd, ...)
    }
    if( NLCD){
        plot.nlcd(x, ...)
    }else{
        plot(x, ...)
    }
    title(main = title);
    if(ipng){
        dev.off()
    }
    if(iasc){
        path.asc=file.path(path, 'Dataout')
        dir.create(path.asc, recursive=TRUE, showWarnings=FALSE)
        if(!grepl('.asc$', fn)){
            filename=paste0(fn,'.asc')
        }else{
            filename= fn;
        }
        writeRaster(filename=file.path(path.asc,filename), x=x, overwrite=TRUE)
    }
}

plot.nlcd <- function(x, legend.text='Land Use',legend=TRUE, code.only=FALSE,
                      grid=TRUE, leg.position = c(0.65, 0.67,0.1,0.4), ...){
        ulc=unique(x)
        nlc = length(ulc)
        col = NLCD.colors(ulc)
        nm = NLCD.names(ulc)
        rcl = cbind(ulc, 1:nlc)
        y= reclassify(x, rcl)
        brk = 0:nlc
        brk.at = 1:nlc -0.5
        if( code.only){
            arg <- list(at=brk.at, labels=paste(ulc))
        }else{
            arg <- list(at=brk.at, labels=paste(ulc, nm))
        }
        legend.args=list(text=legend.text, side=1, font=2, line=2.5, cex=0.8)
        plot(y, col=col,legend = FALSE,...)
        
        if( legend){
            plot(y, col=col, breaks=brk, axis.args=arg, legend.args=legend.args,
                 legend = legend, smallplot=leg.position, legend.only=TRUE,... )
        }
        
        if(grid){
            grid();
        }
}

png2gif <- function(pattern, path.in='./', delay=50, path.out=path.in,filename='Animation.gif'){
    cmd = paste('convert -delay ', delay,
                '-loop 0 ', 
                paste0(path.in, '/', pattern),
               file.path(path.out, filename )
               )
    system ( cmd)  
}
