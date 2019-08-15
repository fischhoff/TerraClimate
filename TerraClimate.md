TerraClimate
================

#### install and load libraries

``` r
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)    
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("raster")

for (package in neededPackages){pkgTest(package)}
```

    ## Loading required package: sp

#### define function getTerra. source: <https://rdrr.io/github/MoisesExpositoAlonso/rbioclim/src/R/getTerra.R>

``` r
#' getTerra
#' Query and load into R data from http://www.climatologylab.org/terraclimate.html
#'
#' @param start start
#' @param end
#' @param byyear
#' @param listyears Instead of start, end, and byyer, user can provide the list of year
#' @param path by default, "data"
#' @variableclim either tmax, tmin, ppt
#'
#' @return list of rasterstack
#' @export
#'
#'
getTerra<-function(start=1958,end=2017,byyear=10,listyears=NULL,variableclim='ppt',path='data'){
  stopifnot(variableclim %in% c('tmax','tmin','ppt'))
  stopifnot(start>=1958)
  stopifnot(end<2019)#originally was 2018
  stopifnot(start<=end)#originally <
  stopifnot(byyear>=1 )
  if(is.null(listyears)) listyears<-seq(start,end,byyear)

  require(raster)

  # helper functions
   # .query<-function(terrafile,path){
   #    message("Downloading terraclimate layer ", terrafile)
   #    cmd<-paste0('wget / https://climate.northwestknowledge.net/TERRACLIMATE-DATA/',terrafile,' ')
   #    system(cmd)
   # }
 .download <- function(path,terrafile, downloadmethod="wget") {
      aurl=paste0('https://climate.northwestknowledge.net/TERRACLIMATE-DATA/',gsub(pattern = "\\.nc$", "", terrafile))
      filename<-paste0(path,"/",terrafile)
      fn <- paste(tempfile(), '.download', sep='')
      res <- utils::download.file(url=aurl, destfile=fn, method=downloadmethod, quiet = FALSE, mode = "wb", cacheOK = TRUE) # other people and me problem with method="auto"
      if (res == 0) {
        w <- getOption('warn')
        on.exit(options('warn' = w))
        options('warn'=-1)
        if (! file.rename(fn, filename) ) {
          # rename failed, perhaps because fn and filename refer to different devices
          file.copy(fn, filename)
          file.remove(fn)
        }
        } else {
          stop('could not download the file' )
        }
      }
  .files<-function(path,variableclim,listyears){
      return(paste0("TerraClimate_",variableclim,"_",listyears,".nc"))
  }
  .checkfiles<-function(path,terrafile){
    foundfiles<-list.files(path = path,pattern = paste0("TerraClimate_"),full.names = T)
    if(paste0(path,"/",terrafile) %in% foundfiles){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  .read<-function(terrafiles){
    rall = lapply(terrafiles,function(myr) {
      message(myr)
      rtmp=stack(lapply(1:12, function(i) raster(myr,band=i,ncdf=TRUE)))
      crs(rtmp)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      return(rtmp)})
    return(rall)
  }
  .getTerra<-function(...){
    terrafiles<-.files(path,variableclim,listyears)
    for (i in terrafiles){
      if(!.checkfiles(path,i)){
        .download(path,i)
      }
    }
    r<-.read(paste0(path,"/",terrafiles))
    # message('Saving .gri/.grd files into ',paste0("data/terraclimate-",variableclim))
    # raster::writeRaster(r,filename = paste0("data/terraclimate-",variableclim), overwrite=T)
    return(r)
    }
  # action
  r<-.getTerra(start,end,byyear,listyears,variableclim,path)
  message('Done')
  return(r)
}
```

#### use getTerra to download precipitation ("ppt") data. Note: each year's file is ~58 MB.

``` r
getTerra(start=1990,end=2018,byyear=1,listyears=NULL,variableclim='ppt',path='data_terra_climate')
```

    ## data_terra_climate/TerraClimate_ppt_1990.nc

    ## Loading required namespace: ncdf4

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1991.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1992.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1993.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1994.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1995.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1996.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1997.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1998.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_1999.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2000.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2001.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2002.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2003.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2004.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2005.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2006.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2007.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2008.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2009.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2010.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2011.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2012.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2013.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2014.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2015.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2016.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2017.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## data_terra_climate/TerraClimate_ppt_2018.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

    ## Done

    ## [[1]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[2]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[3]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[4]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[5]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[6]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[7]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[8]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[9]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[10]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[11]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[12]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[13]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[14]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[15]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[16]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[17]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[18]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[19]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[20]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[21]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[22]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[23]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[24]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[25]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[26]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[27]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[28]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12 
    ## 
    ## 
    ## [[29]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : precipitation_amount.1, precipitation_amount.2, precipitation_amount.3, precipitation_amount.4, precipitation_amount.5, precipitation_amount.6, precipitation_amount.7, precipitation_amount.8, precipitation_amount.9, precipitation_amount.10, precipitation_amount.11, precipitation_amount.12

#### use getTerra to download temperature ("tmax") data. Note: each year's file is ~58 MB.

``` r
getTerra(start=1990,end=2018,byyear=1,listyears=NULL,variableclim='tmax',path='data_terra_climate')
```

    ## data_terra_climate/TerraClimate_tmax_1990.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1991.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1992.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1993.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1994.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1995.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1996.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1997.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1998.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_1999.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2000.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2001.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2002.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2003.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2004.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2005.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2006.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2007.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2008.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2009.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2010.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2011.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2012.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2013.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2014.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2015.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2016.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2017.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## data_terra_climate/TerraClimate_tmax_2018.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmax
    ## If that is not correct, you can set it to one of: tmax, station_influence

    ## Done

    ## [[1]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[2]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[3]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[4]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[5]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[6]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[7]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[8]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[9]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[10]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[11]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[12]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[13]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[14]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[15]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[16]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[17]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[18]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[19]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[20]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[21]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[22]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[23]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[24]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[25]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[26]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[27]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[28]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[29]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12

#### use getTerra to download temperature ("tmin") data. Note: each year's file is ~58 MB.

``` r
getTerra(start=1990,end=2018,byyear=1,listyears=NULL,variableclim='tmin',path='data_terra_climate')
```

    ## data_terra_climate/TerraClimate_tmin_1990.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1991.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1992.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1993.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1994.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1995.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1996.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1997.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1998.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_1999.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2000.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2001.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2002.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2003.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2004.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2005.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2006.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2007.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2008.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2009.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2010.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2011.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2012.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2013.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2014.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2015.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2016.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2017.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## data_terra_climate/TerraClimate_tmin_2018.nc

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Warning in .varName(nc, varname, warn = warn): varname used is: tmin
    ## If that is not correct, you can set it to one of: tmin, station_influence

    ## Done

    ## [[1]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[2]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[3]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[4]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[5]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[6]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[7]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[8]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[9]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[10]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[11]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[12]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[13]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[14]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[15]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[16]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[17]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[18]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[19]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[20]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[21]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[22]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[23]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[24]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[25]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[26]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[27]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[28]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12 
    ## 
    ## 
    ## [[29]]
    ## class       : RasterStack 
    ## dimensions  : 4320, 8640, 37324800, 12  (nrow, ncol, ncell, nlayers)
    ## resolution  : 0.04166667, 0.04166667  (x, y)
    ## extent      : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
    ## coord. ref. : +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 
    ## names       : air_temperature.1, air_temperature.2, air_temperature.3, air_temperature.4, air_temperature.5, air_temperature.6, air_temperature.7, air_temperature.8, air_temperature.9, air_temperature.10, air_temperature.11, air_temperature.12

#### read in one netCDF file as raster and plot

``` r
#use brick to read in all the layers
r = brick("data_terra_climate/TerraClimate_ppt_2018.nc")
```

    ## Warning in .varName(nc, varname, warn = warn): varname used is: ppt
    ## If that is not correct, you can set it to one of: ppt, station_influence

``` r
plot(r[[1]])
```

![](TerraClimate_files/figure-markdown_github/read_plot_one-1.png)
