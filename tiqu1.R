setwd("C:/Users/xupeng/Desktop/HNDenv/data/Changjiang")
library(ncdf4)
library(oce)
library(ocedata)


lonq1=111.70
lonq2=111.75
latq1=16.50
latq2=16.60

n1=(((19.431589-latq1)/0.0416666679084301)+1)
n2=(((19.431589-latq2)/0.0416666679084301)+1)
n3=(((lonq1+108.809419)/0.0416666679084301)+1)
n4=(((lonq2+108.809419)/0.0416666679084301)+1)
lat_d=n1-n2+1
lon_d=n4-n3+1

nc = nc_open("cmems_mod_glo_phy-so_anfc_0.083deg_PT6H-i_1757060920834.nc")
print(nc)

library(raster)
ncfile <- "cmems_mod_glo_phy-so_anfc_0.083deg_PT6H-i_1757060920834.nc"
dset01 <- raster(ncfile)
dset01
plot(dset01)

chlor <- ncvar_get(nc = nc, varid = 'chlor_a', start = c(n3,n2), count = c(lon_d,lat_d))