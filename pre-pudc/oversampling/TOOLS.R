# =============================================================================
# 								Can we open a nc field
#						 Check_Valid_NC_Field(nc,field_ID)
# =============================================================================
Check_Valid_NC_Field <- function(nc, variable_index){
  flag <- NA
  flag <- tryCatch( { dim_temp = !is.na(dim(ncvar_get(nc, nc$var[[variable_index]]))[1]) } , 
                    error = function(w) { result = F; return(result) })
  return(flag)
}

# =============================================================================
# 								Find common elments
#								    common(v1,v2)
# =============================================================================
common <- function(v1, v2){
  r1 <- rle(sort(v1))
  r2 <- rle(sort(v2))
  vals <- intersect(r1$values, r2$values)
  l1 <- r1$lengths[r1$values %in% vals]
  l2 <- r2$lengths[r2$values %in% vals]
  rep(vals, pmin(l1, l2))
}

# =============================================================================
# 								     Find cell
#								    find_cell(x,res,x_start)
# =============================================================================
find_cell <- function(x,res,x_start){
	diff		<-	x-x_start
	cell		<- 	c()
	for(i in 1:length(diff)){
		if( res*diff[i]%/%res == diff[i] ){
			temp	<- diff[i]/res
			if(temp==0){temp<-1}
			cell	<- c(cell,temp)
		}else{
			temp	<- floor(diff[i]/res)+1
			cell	<- c(cell,temp)		
		}
	}
	return(cell)
}

# =============================================================================
# 								     cal_dis
#								    cal_dis(lat1,lat2,lon1,lon2)
# =============================================================================
# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2){
	rad <- pi/180
	a1 <- lat1 * rad
	a2 <- long1 * rad
	b1 <- lat2 * rad
	b2 <- long2 * rad
	dlon <- b2 - a2
	dlat <- b1 - a1
	a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))
	R <- 6378.145
	d <- R * c
	return(d)
}

# =============================================================================
# 								     cal_lat_lon
#								    cal_lat_lon(lat,lon,degree,distance)
# =============================================================================
cal_lat_lon	<- function(lat,lon,degree,distance){
	distance_east	<- distance*sin(degree/180*pi) #km
	dlon			<- distance_east/earth.dist(lon,lat,lon+1,lat)
	lon_new			<- lon+dlon
	distance_north	<- distance*cos(degree/180*pi) #km
	dlat			<- distance_north/earth.dist(lon,lat,lon,lat+1)
	lat_new			<- lat+dlat
	return(c(lat_new,lon_new))
}

#************************************************************************
# Used to cal HCHO from VOCs
#************************************************************************
cal_column_VOC  <- function(E,Y,k_HCHO,k,u,L,x){
  column_L_VOC	<- E*Y*( (k_HCHO*exp(-k*L/u)-k*exp(-k_HCHO*L/u))/(k_HCHO*(k-k_HCHO)) + 1/k_HCHO)
  if(x<=L){
    column_VOC	<- E*Y*( (k_HCHO*exp(-k*x/u)-k*exp(-k_HCHO*x/u))/(k_HCHO*(k-k_HCHO)) + 1/k_HCHO)
  }else{
    x_cor	<- x-L
    column_VOC	<- column_L_VOC*exp(-k_HCHO*x_cor/u) + Y*E*(1-exp(-k*L/u))/(k-k_HCHO)*(exp(-k_HCHO*x_cor/u)-exp(-k*x_cor/u))
  }
  return(column_VOC)
}

#************************************************************************
# Used to cal HCHO from primaray HCHO
#************************************************************************
cal_column_HCHO	<- function(E,k_HCHO,u,L,x){
  column_L_HCHO		<- E/k_HCHO*(1-exp(-k_HCHO*L/u))
  if(x<=L){
    column_HCHO	<- E/k_HCHO*(1-exp(-k_HCHO*x/u))
  }else{
    x_cor	<- x-L
    column_HCHO	<- column_L_HCHO*exp(-k_HCHO*x_cor/u)
  }
}

#************************************************************************
# Get the k based on OH conc. and T, P and species
#************************************************************************
get_OH_k		<- function(Temperature,Pressure,Species,OH){
  # Propylene, C3H6
  if(Species==1){
    M		<- 7.243e21*Pressure/Temperature
    K160	<- 8.0e-27*M*(Temperature/300)^(-3.5)
    K161	<- 3.0e-11*(Temperature/300)^(-1)
    KR16	<- K160/K161
    FC16	<- 0.5
    NC16	<- 0.75-1.27*log10(FC16)
    F16		<- 10^(log10(FC16)/(1+(log10(KR16)/NC16)^2))
    KMT16	<- K160*K161*F16/(K160+K161)
    k		<- KMT16*OH
    return(k)
  }
  # Ethylene, C2H4
  if(Species==2){
    M		<- 7.243e21* Pressure/Temperature
    K150	<- 8.6e-29*M*(Temperature/300)^(-3.1)
    K151	<- 9.0e-12*(Temperature/300)^(-0.85)
    KR15	<- K150/K151
    FC15	<- 0.48
    NC15	<- 0.75-1.27*log10(FC15)
    F15		<- 10^(log10(FC15)/(1+(log10(KR15)/NC15)^2))
    KMT15	<- K150*K151*F15/(K150+K151)
    k		<- KMT15*OH
    return(k)		
  }
  # HCHO
  if(Species==3){
    k	<- 5.4e-12*exp(135/Temperature)*OH
    return(k)
  }
  # 1-Butene
  if(Species==4){
    k	<- 6.6e-12*exp(465/Temperature)*OH
    return(k)
  }
  # 1,3-Butadiene
  if(Species==5){
    k	<- 1.48e-11*exp(448/Temperature)*(0.649+0.134)*OH
    return(k)
  }
  # Isobutylene
  if(Species==6){
    k	<- 9.4e-12*exp(505/Temperature)*OH
    return(k)
  }
  # Diethyl Ether
  if(Species==7){
    k	<- 8.91e-18*Temperature^2*exp(837/Temperature)*OH
    return(k)
  }
  # n-Butanol
  if(Species==8){
    k	<- 5.3e-12*exp(140/Temperature)*(0.321+0.321)*OH
    return(k)
  }
  # Isopropanol
  if(Species==9){
    k	<- 2.6e-12*exp(200/Temperature)*0.139*OH
    return(k)
  }
  # Acetaldehyde
  if(Species==10){
    k	<- 4.7e-12*exp(345/Temperature)*OH
    return(k)
  }
}

#************************************************************************
# Covert julian second to hour
#************************************************************************
get_hour	<- function(JS){
  if(JS<=0){
    JS	<- JS+3600*24
  }	
  if((JS%/%3600)*3600==JS){
    hour	<- JS%/%3600-1
  }else{
    hour	<- floor(JS/(3600))
  }
  return(hour)
}

#************************************************************************
# Linear Interplot
#************************************************************************
linear_inter <- function(time, time_p, time_n, value_p, value_n){
  value <- (time-time_p)/(time_p-time_n)*(value_p-value_n)+value_p
  return(value)
}

#************************************************************************
# Read ICT text file
#************************************************************************
read_ICT      <- function(filename){
  strs        <- readLines(filename)
  header_skip <- as.numeric(strsplit(x=strs[1],split=",")[[1]][1])-1
  dat         <- read.csv(text=strs,skip=header_skip,nrows=length(strs),header=T)
  return(dat)
}

#************************************************************************
# Convert UTC second to HHMMSS
#************************************************************************
convert_UTC_second      <- function(UTC_second){
  HH  <- floor(UTC_second/3600)
  MM  <- floor((UTC_second-HH*3600)/60)
  SS  <- UTC_second-MM*60-HH*3600
  return(c(HH,MM,SS))
}

#************************************************************************
# Convert UTC second to HHMMSS
#************************************************************************
point_in_poly <- function(point_lon,point_lat,lon,lat){
  library(SDMTools)
  point_in <- 0
  pnts            <- cbind(x=point_lon,y=point_lat)
  
  polypnts = cbind(x=c(lon[1],lon[2],lon[3],lon[4]),y=c(lat[1],lat[2],lat[3],lat[4]))
  out1 = pnt.in.poly(pnts,polypnts)
  
  polypnts = cbind(x=c(lon[1],lon[2],lon[4],lon[3]),y=c(lat[1],lat[2],lat[4],lat[3]))
  out2 = pnt.in.poly(pnts,polypnts)
  
  polypnts = cbind(x=c(lon[1],lon[3],lon[2],lon[4]),y=c(lat[1],lat[3],lat[2],lat[4]))
  out3 = pnt.in.poly(pnts,polypnts)
  
  polypnts = cbind(x=c(lon[1],lon[3],lon[4],lon[2]),y=c(lat[1],lat[3],lat[4],lat[2]))
  out4 = pnt.in.poly(pnts,polypnts)
  
  polypnts = cbind(x=c(lon[1],lon[4],lon[2],lon[3]),y=c(lat[1],lat[4],lat[2],lat[3]))
  out5 = pnt.in.poly(pnts,polypnts)
  
  polypnts = cbind(x=c(lon[1],lon[4],lon[3],lon[2]),y=c(lat[1],lat[4],lat[3],lat[2]))
  out6 = pnt.in.poly(pnts,polypnts)
  
  temp <- out1$pip+out2$pip+out3$pip+out4$pip+out5$pip+out6$pip
  
  if(temp>=1){
    point_in <- 1
  }
  
  return(point_in)
}

#************************************************************************
# Determine how many days in a month
#************************************************************************
DaysinMonth <- function(year,month){
  library(chron)
  days_leap <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  days      <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(leap.year(year)){
    Ndays <- days_leap[month]
  }else{
    Ndays <- days[month]
  }
  return(Ndays)
}


#************************************************************************
# Convert integer to character, with 0 heading
#************************************************************************ 
I2C  <- function(x){
  temp	<- as.character(x)
  if(x<10){
    temp	<- paste("0",temp,sep="")
  }
  return(temp)
}

#************************************************************************
# Convert Time to HH MM SS SSSS
#************************************************************************ 
Get_Time <- function(t){
  hh   <- floor(t)
  mm   <- floor((t-hh)*60)
  ss   <- floor((t-hh-mm/60)*3600)
  ssss <- (t-hh-mm/60-ss/3600)*3600*1000
  return(c(hh,mm,ss,ssss))
}


#************************************************************************
# Get r based on two vectors which can contain NAs
#************************************************************************ 
Cal_r <- function(vector1, vector2){
  common1 <- c()
  common2 <- c()
  if(length(vector1)!=length(vector2)){
    print("Lists are not at the same length!!")
  }else{
    for( i in 1:length(vector1)){
      if(!is.na(vector1[i]) && !is.na(vector2[i])){
        common1 <- c(common1, vector1[i])
        common2 <- c(common2, vector2[i])
      }
    }
  }
  return(cor(common1,common2))
}


#************************************************************************
# Define error bar functions
#************************************************************************

error.bar.v <- function(x, y, upper, col,lower=upper, length=0.03,lwd=1.5,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, col=col,length=length, lwd=lwd,...)
}

error.bar.h <- function(x, y, righter, col,lower=righter, length=0.03,lwd=1.5,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(righter))
    stop("vectors must be same length")
  arrows(x-righter,y, x+righter,y, angle=270, code=3, col=col,length=length, lwd=lwd,...)
}
