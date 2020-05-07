# Declining Fertility in Taiwan: The Deterring Impact of Housework Imbalance
# Data preparation script: hybrid models from 2010, cross-sectional
# models from 2010
# Tuesday 5th May 2020

# ------------------------------------------------------------------

# CONTENTS
# 1. Define functions
# 2. Read in data and preliminary subsetting
# 3. Add in information on childbearing
# 4. Build model frame and create Stata .dat files

# ------------------------------------------------------------------

# 1. Define functions

weduH <- function(x,edu,sedu){
  code <- as.integer(substr(x[,edu],1,2))
  scode <- as.integer(substr(x[,sedu],1,2))
  weduH <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="2 ¤k©Ê"){
      if (code[i]>8){
        weduH[i] <- 1
      }
    } else {
      if (scode[i]>8){
        weduH[i] <- 1
      }
    }
  }
  weduH
} # Wife's higher education binary
heduH <- function(x,edu,sedu){
  code <- as.integer(substr(x[,edu],1,2))
  scode <- as.integer(substr(x[,sedu],1,2))
  heduH <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="1 ¨k©Ê"){
      if (code[i]>8){
        heduH[i] <- 1
      }
    } else {
      if (scode[i]>8){
        heduH[i] <- 1
      }
    }
  }
  heduH
} # Husband's higher education binary
whw <- function(x,hw,shw){
  whw <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="1 ¨k©Ê"){
      whw[i] <- x[i,shw]
    } else {
      whw[i] <- x[i,hw]
    }
  }
  whw
} # Wife's weekly hours of housework
hhw <- function(x,hw,shw){
  hhw <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="1 ¨k©Ê"){
      hhw[i] <- x[i,hw]
    } else {
      hhw[i] <- x[i,shw]
    }
  }
  hhw
} # Husband's weekly hours of housework
w_wrkhrs <- function(x,res,spouse){
  w_wrkhrs <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="1 ¨k©Ê"){
      w_wrkhrs[i] <- x[i,spouse]
    } else {
      w_wrkhrs[i] <- x[i,res]
    }
  }
  w_wrkhrs
} # Wife's weekly work hours
h_wrkhrs <- function(x,res,spouse){
  h_wrkhrs <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="1 ¨k©Ê"){
      h_wrkhrs[i] <- x[i,res]
    } else {
      h_wrkhrs[i] <- x[i,spouse]
    }
  }
  h_wrkhrs
} # Husband's weekly work hours
parentsb <- function(x,m,d,sm,sd){
  parentsb <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (class(x[,m]) != "numeric"){
      mn <- as.numeric(substr(x[i,m],1,1))
      dn <- as.numeric(substr(x[i,d],1,1))
      smn <- as.numeric(substr(x[i,sm],1,1))
      sdn <- as.numeric(substr(x[i,sd],1,1))
      if (mn==1 | dn==1 | smn==1 | sdn==1){
        parentsb[i] <- 1
      }
    } else {
      if (x[i,m]==1 | x[i,d]==1 | x[i,sm]==1 | x[i,sd]==1){
        parentsb[i] <- 1
      }
    }
  }
  parentsb
} # Parents' co-residence binary
w_inc <- function(x,inc,incc,sinc,sincc){
  w_inc <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="2 ¤k©Ê"){
      if (x[i,inc] < 300001){
        w_inc[i] <- x[i,inc]
      } else {
        cat <- as.numeric(substr(x[i,incc],1,2))
        if (cat == 0){
          w_inc[i] <- 0
        } else if (cat == 1){
          w_inc[i] <- 5000
        } else if ((cat > 1) & (cat < 22)){
          w_inc[i] <- (((cat-1)+(cat-2))/2)*10000
        } else if (cat == 22){
          w_inc[i] <- 250000
        } else if (cat == 23){
          w_inc[i] <- 300000
        }
      }
    } else {
      if (x[i,sinc] < 200001){
        w_inc[i] <- x[i,sinc]
      } else {
        cat <- as.numeric(substr(x[i,sincc],1,2))
        if (cat == 0){
          w_inc[i] <- 0
        } else if (cat == 1){
          w_inc[i] <- 5000
        } else if ((cat > 1) & (cat < 22)){
          w_inc[i] <- (((cat-1)+(cat-2))/2)*10000
        } else if (cat == 22){
          w_inc[i] <- 250000
        } else if (cat == 23){
          w_inc[i] <- 300000
        }
      }
    } 
  }
  w_inc
} # Wife's monthly income
h_inc <- function(x,inc,incc,sinc,sincc){
  h_inc <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="1 ¨k©Ê"){
      if (x[i,inc] < 300001){
        h_inc[i] <- x[i,inc]
      } else {
        cat <- as.numeric(substr(x[i,incc],1,2))
        if (cat == 0){
          h_inc[i] <- 0
        } else if (cat == 1){
          h_inc[i] <- 5000
        } else if ((cat > 1) & (cat < 22)){
          h_inc[i] <- (((cat-1)+(cat-2))/2)*10000
        } else if (cat == 22){
          h_inc[i] <- 250000
        } else if (cat == 23){
          h_inc[i] <- 300000
        }
      }
    } else {
      if (x[i,sinc] < 200001){
        h_inc[i] <- x[i,sinc]
      } else {
        cat <- as.numeric(substr(x[i,sincc],1,2))
        if (cat == 0){
          h_inc[i] <- 0
        } else if (cat == 1){
          h_inc[i] <- 5000
        } else if ((cat > 1) & (cat < 22)){
          h_inc[i] <- (((cat-1)+(cat-2))/2)*10000
        } else if (cat == 22){
          h_inc[i] <- 250000
        } else if (cat == 23){
          h_inc[i] <- 300000
        }
      }
    } 
  }
  h_inc
} # Husband's monthly income
gender <- function(x){
  y <- as.character(x$a01)
  y[y == "1 ¨k©Ê"] <- "male"
  y[y == "2 ¤k©Ê"] <- "female"
  as.factor(y)
} # Gender of respondent (vector)
nanb <- function(x,variable){
  nanb <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,variable]>0){
      nanb[i] <- 1
    }
  }
  nanb
} # Pays for nanny binary
morebirb <- function(x,bintention){
  morbirb <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,bintention]=="1 ¦³"){
      morbirb[i] <- 1
    }
  }
  morbirb
} # Intention for another birth 
# can take any data frame as an argument, except ri2009
morebirb_ri09 <- function(x){
  morebirb <- integer(nrow(x))
  for (i in 1:nrow(x)){
    row <- which(rr2010[,"x01"]==x[i,"x01"])
    if (rr2010[row,"e03"]=="1 ¦³"){
      morebirb[i] <- 1
    }
  }
  morebirb
} # Impute morebirb for ri2009 from rr2010 values
wy_age <- function(x,syear,yob,syob){
  wy_age <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (x[i,"a01"]=="2 ¤k©Ê"){
      wy_age[i] <- syear - (x[i,yob] + 1911)
    } else {
      wy_age[i] <- syear - (x[i,syob] + 1911)
    }
  }
  wy_age
} # Wife's age in years
wy_plus <- function(x,y,wy_age,step){
  vec <- integer(nrow(y))
  for (i in 1:nrow(y)){
    if (y[i,"x01"] %in% x[,"x01"]){
      row <- which(x[,"x01"]==y[i,"x01"])
      vec[i] <- x[row,wy_age] + step
    }
  }
  vec
} # Wife's age in the next year
genideo <- function(x){
  d02a <- as.numeric(substr(x[,"d02a"],1,1))
  d02b <- as.numeric(substr(x[,"d02b"],1,1))
  d02d <- as.numeric(substr(x[,"d02d"],1,1))
  d02e <- as.numeric(substr(x[,"d02e"],1,1))
  d02f <- as.numeric(substr(x[,"d02f"],1,1))
  d02a. <- integer(nrow(x))
  d02f. <- integer(nrow(x))
  for (i in 1:nrow(x)){
    if (d02a[i]==1){
      d02a.[i] <- 5
    } else if (d02a[i]==2){
      d02a.[i] <- 4
    } else if (d02a[i]==3){
      d02a.[i] <- 3
    } else if (d02a[i]==4){
      d02a.[i] <- 2
    } else if (d02a[i]==5){
      d02a.[i] <- 1
    }
  }
  for (i in 1:nrow(x)){
    if (d02f[i]==1){
      d02f.[i] <- 5
    } else if (d02f[i]==2){
      d02f.[i] <- 4
    } else if (d02f[i]==3){
      d02f.[i] <- 3
    } else if (d02f[i]==4){
      d02f.[i] <- 2
    } else if (d02f[i]==5){
      d02f.[i] <- 1
    }
  }
  genideo <- (d02a. + d02b + d02d + d02e + d02f.) / 5
  genideo
} # Gender ideology - can only take rr2010 as an argument.
genideo_lookup <- function(x){
  vec <- integer(nrow(x))
  for (i in 1:nrow(rr10)){
    row <- which(x[,"x01"]==rr10[i,"x01"])
    vec[row] <- rr10[i,"genideo"]
  }
  vec
} # Gender ideology, using rr10 values. Must make
# rr10 frame first, and must make x frame first.

# ------------------------------------------------------------------

# 2. Read in data and preliminary subsetting

# to run this on your computer, change the working directory to the directory where you want
# to output the stata .dta files to.

library(foreign)
setwd("C:/Users/Jac/OneDrive/paper_1/submission_3/analysis_and_results")

# to run this on your computer, read in raw .dta data from a local directory.

# ri2009 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00284_2/temp/ri2009_c_v201305_stata.dta")
# rr2009 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00284_1/temp/RR2009_c_V201212_stata.dta")
rr2010 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00297_1/temp/RR2010_c_v201308_stata.dta")
rr2011 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00298_1/temp/rr2011_c_v201309_stata.dta")
rr2012 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00316_1/RR2012/rr2012_v201605_stata.dta")
rr2014 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00318_1/RR2014_v201607_stata.dta")
rr2016 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00320_1/RR2016_v201801_stata.dta")

# rr2016
# 3 and 4 are the 2003 and 2009 respondents.
rr2016 <- subset.data.frame(rr2016,x01b=="3 1964-76¦~¥X¥Í¤§¥D¼Ë¥»"|x01b=="4 1977-83¦~¥X¥Í¤§¥D¼Ë¥»")
# Remove those without children in 2016 (since they would not have
# had children in 2010.)
rr2016 <- subset.data.frame(rr2016,b15 != 0)

# rr2010
# Remove those who aren't first married
rr2010 <- subset.data.frame(rr2010,a16z01=="03 ¤w±B(²Ä¤@¦¸µ²±B)")
# Subset those respondents in rr2010 who are in surveys from all other years.
rr2010 <- subset.data.frame(rr2010,rr2010[,"x01"] %in% rr2011[,"x01"] &
                              rr2010[,"x01"] %in% rr2012[,"x01"] &
                              rr2010[,"x01"] %in% rr2014[,"x01"] &
                              rr2010[,"x01"] %in% rr2016[,"x01"])

# rr2011
# Subset those respondents in rr2011 who are in rr2010
rr2011 <- subset.data.frame(rr2011,rr2011[,"x01"] %in% rr2010[,"x01"])
# Remove those who aren't still married.
rr2011 <- subset.data.frame(rr2011,a16a=="01 ¨S¦³ÅÜ¤Æ;¤´¬O¤w±B")

# rr2012
# Subset those respondents in rr2012 who are in rr2011
rr2012 <- subset.data.frame(rr2012,rr2012[,"x01"] %in% rr2011[,"x01"])
# Remove those who aren't still married.
rr2012 <- subset.data.frame(rr2012,a16a=="1 ¨S¦³ÅÜ¤Æ;¤´¬O¤w±B")

# rr2014
# Subset those respondents in rr2014 who are in rr2012
rr2014 <- subset.data.frame(rr2014,rr2014[,"x01"] %in% rr2012[,"x01"])
# Remove those who aren't still married.
rr2014 <- subset.data.frame(rr2014,a16a=="1 ¨S¦³ÅÜ¤Æ;¤´¬O¤w±B(¸õ¦ÜA17)")

# Subset the rr2016 respondents by those that are in all previous surveys,
# then subset each previous survey by those that are in rr2016, thereby
# yielding a balanced panel.

rr2016 <- subset.data.frame(rr2016,rr2016[,"x01"] %in% rr2010[,"x01"] &
                              rr2016[,"x01"] %in% rr2011[,"x01"] &
                              rr2016[,"x01"] %in% rr2012[,"x01"] &
                              rr2016[,"x01"] %in% rr2014[,"x01"])

rr2010 <- subset.data.frame(rr2010,rr2010[,"x01"] %in% rr2016[,"x01"])
rr2011 <- subset.data.frame(rr2011,rr2011[,"x01"] %in% rr2016[,"x01"])
rr2012 <- subset.data.frame(rr2012,rr2012[,"x01"] %in% rr2016[,"x01"])
rr2014 <- subset.data.frame(rr2014,rr2014[,"x01"] %in% rr2016[,"x01"])

# This leaves 753 observations.

# ------------------------------------------------------------------

# 3. Add in information on childbearing

# Someone ranked their eldest child 2nd accidentally (person 41110); amend that:
which(rr2016[,"x01"]==41110)
rr2016[22,"b16ac1"] <- 1

# rr2016 kids birthdays - years only
rr2016 <- rr2016[rr2016[,"b16c01c1"]<996,]
rr2016 <- rr2016[rr2016[,"b16c01c2"]<996,]
rr2016 <- rr2016[rr2016[,"b16c01c3"]<996,]
rr2016 <- rr2016[rr2016[,"b16c01c4"]<996,]
rr2016 <- rr2016[rr2016[,"b16c01c5"]<996,]
rr2016 <- rr2016[rr2016[,"b16c01c6"]<996,]

rr2016[,"c1YOB"] <- rr2016[,"b16c01c1"] + 1911
rr2016[,"c2YOB"] <- rr2016[,"b16c01c2"] + 1911
rr2016[,"c3YOB"] <- rr2016[,"b16c01c3"] + 1911
rr2016[,"c4YOB"] <- rr2016[,"b16c01c4"] + 1911
rr2016[,"c5YOB"] <- rr2016[,"b16c01c5"] + 1911
rr2016[,"c6YOB"] <- rr2016[,"b16c01c6"] + 1911

for (i in 1:nrow(rr2016)){
  if (rr2016[i,"c1YOB"]==1911){
    rr2016[i,"c1YOB"] <- NA
  }
}
for (i in 1:nrow(rr2016)){
  if (rr2016[i,"c2YOB"]==1911){
    rr2016[i,"c2YOB"] <- NA
  }
}
for (i in 1:nrow(rr2016)){
  if (rr2016[i,"c3YOB"]==1911){
    rr2016[i,"c3YOB"] <- NA
  }
}
for (i in 1:nrow(rr2016)){
  if (rr2016[i,"c4YOB"]==1911){
    rr2016[i,"c4YOB"] <- NA
  }
}
for (i in 1:nrow(rr2016)){
  if (rr2016[i,"c5YOB"]==1911){
    rr2016[i,"c5YOB"] <- NA
  }
}
for (i in 1:nrow(rr2016)){
  if (rr2016[i,"c6YOB"]==1911){
    rr2016[i,"c6YOB"] <- NA
  }
}
rm(i)

rr2016[,"coldYOB"] <- pmin(rr2016[,"c1YOB"],rr2016[,"c2YOB"],
                           rr2016[,"c3YOB"],rr2016[,"c4YOB"],
                           rr2016[,"c5YOB"],rr2016[,"c6YOB"],na.rm = TRUE)
rr2016[,"cyoungYOB"] <- pmax(rr2016[,"c1YOB"],rr2016[,"c2YOB"],
                             rr2016[,"c3YOB"],rr2016[,"c4YOB"],
                             rr2016[,"c5YOB"],rr2016[,"c6YOB"],na.rm = TRUE)

kyobs <- data.frame(rr2016[,c("x01","c1YOB","c2YOB","c3YOB","c4YOB","c5YOB","c6YOB","coldYOB","cyoungYOB")])

kyobs <- data.frame(kyobs,b2011=integer(nrow(kyobs)),
                    b2012=integer(nrow(kyobs)),b2013=integer(nrow(kyobs)),
                    b2014=integer(nrow(kyobs)),b2015=integer(nrow(kyobs)))

for (i in 1:nrow(kyobs)){
  if (2011 %in% kyobs[i,c("c1YOB","c2YOB","c3YOB","c4YOB","c5YOB","c6YOB")]){
    kyobs[i,"b2011"] <- 1
  }
}
for (i in 1:nrow(kyobs)){
  if (2012 %in% kyobs[i,c("c1YOB","c2YOB","c3YOB","c4YOB","c5YOB","c6YOB")]){
    kyobs[i,"b2012"] <- 1
  }
}
for (i in 1:nrow(kyobs)){
  if (2013 %in% kyobs[i,c("c1YOB","c2YOB","c3YOB","c4YOB","c5YOB","c6YOB")]){
    kyobs[i,"b2013"] <- 1
  }
}
for (i in 1:nrow(kyobs)){
  if (2014 %in% kyobs[i,c("c1YOB","c2YOB","c3YOB","c4YOB","c5YOB","c6YOB")]){
    kyobs[i,"b2014"] <- 1
  }
}
for (i in 1:nrow(kyobs)){
  if (2015 %in% kyobs[i,c("c1YOB","c2YOB","c3YOB","c4YOB","c5YOB","c6YOB")]){
    kyobs[i,"b2015"] <- 1
  }
}

# cnum for each year.

kyobs <- data.frame(kyobs,c1YOB9=kyobs[,"c1YOB"],c2YOB9=kyobs[,"c2YOB"],c3YOB9=kyobs[,"c3YOB"],c4YOB9=kyobs[,"c4YOB"],c5YOB9=kyobs[,"c5YOB"],c6YOB9=kyobs[,"c6YOB"])

kyobs[,"c1YOB9"][is.na(kyobs[,"c1YOB9"])] <- 9999
kyobs[,"c2YOB9"][is.na(kyobs[,"c2YOB9"])] <- 9999
kyobs[,"c3YOB9"][is.na(kyobs[,"c3YOB9"])] <- 9999
kyobs[,"c4YOB9"][is.na(kyobs[,"c4YOB9"])] <- 9999
kyobs[,"c5YOB9"][is.na(kyobs[,"c5YOB9"])] <- 9999
kyobs[,"c6YOB9"][is.na(kyobs[,"c6YOB9"])] <- 9999

kyobs <- data.frame(kyobs,c2010=(kyobs[,"c1YOB9"]<=2010)+(kyobs[,"c2YOB9"]<=2010)+(kyobs[,"c3YOB9"]<=2010)+(kyobs[,"c4YOB9"]<=2010)+(kyobs[,"c5YOB9"]<=2010)+(kyobs[,"c6YOB9"]<=2010))
kyobs <- data.frame(kyobs,c2011=(kyobs[,"c1YOB9"]<=2011)+(kyobs[,"c2YOB9"]<=2011)+(kyobs[,"c3YOB9"]<=2011)+(kyobs[,"c4YOB9"]<=2011)+(kyobs[,"c5YOB9"]<=2011)+(kyobs[,"c6YOB9"]<=2011))
kyobs <- data.frame(kyobs,c2012=(kyobs[,"c1YOB9"]<=2012)+(kyobs[,"c2YOB9"]<=2012)+(kyobs[,"c3YOB9"]<=2012)+(kyobs[,"c4YOB9"]<=2012)+(kyobs[,"c5YOB9"]<=2012)+(kyobs[,"c6YOB9"]<=2012))
kyobs <- data.frame(kyobs,c2013=(kyobs[,"c1YOB9"]<=2013)+(kyobs[,"c2YOB9"]<=2013)+(kyobs[,"c3YOB9"]<=2013)+(kyobs[,"c4YOB9"]<=2013)+(kyobs[,"c5YOB9"]<=2013)+(kyobs[,"c6YOB9"]<=2013))
kyobs <- data.frame(kyobs,c2014=(kyobs[,"c1YOB9"]<=2014)+(kyobs[,"c2YOB9"]<=2014)+(kyobs[,"c3YOB9"]<=2014)+(kyobs[,"c4YOB9"]<=2014)+(kyobs[,"c5YOB9"]<=2014)+(kyobs[,"c6YOB9"]<=2014))

cyoung10YOB <- integer(nrow(kyobs))
for (i in 1:nrow(kyobs)){
  if (is.na(kyobs[i,"c6YOB"]) == TRUE | kyobs[i,"c6YOB"] > 2010){
    if (is.na(kyobs[i,"c5YOB"]) == TRUE | kyobs[i,"c5YOB"] > 2010){
      if (is.na(kyobs[i,"c4YOB"]) == TRUE | kyobs[i,"c4YOB"] > 2010){
        if (is.na(kyobs[i,"c3YOB"]) == TRUE | kyobs[i,"c3YOB"] > 2010){
          if (is.na(kyobs[i,"c2YOB"]) == TRUE | kyobs[i,"c2YOB"] > 2010){
            cyoung10YOB[i] <- 2010 - kyobs[i,"c1YOB"]
          } else {
            cyoung10YOB[i] <- 2010 - kyobs[i,"c2YOB"]
          }
        } else {
          cyoung10YOB[i] <- 2010 - kyobs[i,"c3YOB"]
        }
      } else {
        cyoung10YOB[i] <- 2010 - kyobs[i,"c4YOB"]
      }
    } else {
      cyoung10YOB[i] <- 2010 - kyobs[i,"c5YOB"]
    }
  } else {
    cyoung10YOB[i] <- 2010 - kyobs[i,"c6YOB"]
  }
}

cyoung11YOB <- integer(nrow(kyobs))
for (i in 1:nrow(kyobs)){
  if (kyobs[i,"b2011"] == 0){
    cyoung11YOB[i] <- cyoung10YOB[i] + 1
  }
}

cyoung12YOB <- integer(nrow(kyobs))
for (i in 1:nrow(kyobs)){
  if (kyobs[i,"b2012"] == 0){
    cyoung12YOB[i] <- cyoung11YOB[i] + 1
  }
}

cyoung13YOB <- integer(nrow(kyobs))
for (i in 1:nrow(kyobs)){
  if (kyobs[i,"b2013"] == 0){
    cyoung13YOB[i] <- cyoung12YOB[i] + 1
  }
}

cyoung14YOB <- integer(nrow(kyobs))
for (i in 1:nrow(kyobs)){
  if (kyobs[i,"b2014"] == 0){
    cyoung14YOB[i] <- cyoung13YOB[i] + 1
  }
}

cyoung15YOB <- integer(nrow(kyobs))
for (i in 1:nrow(kyobs)){
  if (kyobs[i,"b2015"] == 0){
    cyoung15YOB[i] <- cyoung14YOB[i] + 1
  }
}

kyobs <- data.frame(kyobs,cyoung10YOB,cyoung11YOB,cyoung12YOB,cyoung13YOB,cyoung14YOB,cyoung15YOB)
rm(cyoung10YOB,cyoung11YOB,cyoung12YOB,cyoung13YOB,cyoung14YOB,cyoung15YOB)

# Some negative values, but should be OK because those will be removed
# later (basically, those with negative values are not supposed to be
# in the data, i.e. they're not within one of the categories we've
# chosen). Off the top of my head, I can't remember exactly why this
# has happened; but I remember from before that it is explicable, and
# that it doesn't matter.

k11_15b <- data.frame(x01=kyobs[,"x01"],yh=pmax(kyobs[,"b2011"],kyobs[,"b2012"],kyobs[,"b2013"],kyobs[,"b2014"],kyobs[,"b2015"]))
k12_15b <- data.frame(x01=kyobs[,"x01"],yh=pmax(kyobs[,"b2012"],kyobs[,"b2013"],kyobs[,"b2014"],kyobs[,"b2015"]))
k13_15b <- data.frame(x01=kyobs[,"x01"],yh=pmax(kyobs[,"b2013"],kyobs[,"b2014"],kyobs[,"b2015"]))
k14_15b <- data.frame(x01=kyobs[,"x01"],yh=pmax(kyobs[,"b2014"],kyobs[,"b2015"]))
k15_15b <- data.frame(x01=kyobs[,"x01"],yh=pmax(kyobs[,"b2015"]))

rm(i)

# ------------------------------------------------------------------

# 4. Build model frame and create Stata .dat file

# rr10
rr10 <- data.frame(x01=rr2010[,"x01"],syear=2010,
                   weduH=weduH(rr2010,"a03c","a18"),
                   heduH=heduH(rr2010,"a03c","a18"),whw=whw(rr2010,"c01a","c01b"),
                   hhw=hhw(rr2010,"c01a","c01b"),
                   w_wrkhrs=w_wrkhrs(rr2010,"a11a","a26a"),h_wrkhrs=h_wrkhrs(rr2010,"a11a","a26a"),
                   parentsb=parentsb(rr2010,"b04b01","b04b02","b04b04","b04b05"),
                   w_inc=w_inc(rr2010,"a10a","a10b","a25a","a25b"),
                   h_inc=h_inc(rr2010,"a10a","a10b","a25a","a25b"),
                   gender=gender(rr2010),nanb=nanb(rr2010,"c08c"),
                   morebirb=morebirb(rr2010,"e03"),
                   tdum=integer(nrow(rr2010)),ddum=integer(nrow(rr2010)),
                   genideo=genideo(rr2010),
                   wy_age=wy_age(rr2010,2010,"a02a","a17z01"))
rr10 <- rr10[rr10[,"x01"] %in% kyobs[,"x01"],]

birth <- integer(nrow(rr10))
for (i in 1:nrow(rr10)){
  row <- which(kyobs[,"x01"] == rr10[i,"x01"])
  birth[i] <- kyobs[row,"b2011"]
}

numc <- integer(nrow(rr10))
for (i in 1:nrow(rr10)){
  row <- which(kyobs[,"x01"] == rr10[i,"x01"])
  numc[i] <- kyobs[row,"c2010"]
}

coldYOB <- integer(nrow(rr10))
for (i in 1:nrow(rr10)){
  row <- which(kyobs[,"x01"] == rr10[i,"x01"])
  coldYOB[i] <- kyobs[row,"coldYOB"]
}

rr10 <- data.frame(rr10,birth,numc,coldYOB)
rm(birth,numc,coldYOB,i,row)

rr10 <- rr10[rr10[,"numc"]>0,]

# Not all wives are between the ages of 20 to 40; remove those that aren't.

rr10 <- rr10[rr10[,"wy_age"]<41,]

# Some observations report very high values, or 990 values: these are in whw, hhw,
# w_wrkhrs, and h_wrkhrs.

table(rr10[,"whw"]<60)
#FALSE  TRUE 
#60   497 

table(rr10[,"hhw"]<60)
#FALSE  TRUE 
#17   540 

table(rr10[,"w_wrkhrs"]<90)
#FALSE  TRUE 
#4    553 

table(rr10[,"h_wrkhrs"]<90)
#FALSE  TRUE 
#16   541 

# Impute data for these observations from same individual from 2009.

# setwd("C:/Users/Jac/OneDrive/paper_1/submission_3/new_paper_results")
ri2009 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00284_2/temp/ri2009_c_v201305_stata.dta")
rr2009 <- read.dta("C:/Users/Jac/Desktop/data_research/PSFD/chinese/C00284_1/temp/RR2009_c_V201212_stata.dta")

# Append ri2009 and rr2009 with gender vectors.

ri2009 <- data.frame(ri2009,gender=gender(ri2009))
rr2009 <- data.frame(rr2009,gender=gender(rr2009))

for (i in 1:nrow(rr10)){
  if (rr10[i,"whw"]>59){
    if (rr10[i,"x01"] %in% ri2009[,"x01"]){
      row <- which(ri2009[,"x01"]==rr10[i,"x01"])
      if (ri2009[row,"gender"]=="female"){
        rr10[i,"whw"] <- ri2009[row,"h02a"]
      } else {
        rr10[i,"whw"] <- ri2009[row,"h02b"]
      }
    } else if (rr10[i,"x01"] %in% rr2009[,"x01"]){
      row <- which(rr2009[,"x01"]==rr10[i,"x01"])
      if (rr2009[row,"gender"]=="female"){
        rr10[i,"whw"] <- rr2009[row,"c01a"]
      } else {
        rr10[i,"whw"] <- rr2009[row,"c01b"]
      }
    }
  }
}
table(rr10[,"whw"]<60)
#FALSE  TRUE 
#9    548 

for (i in 1:nrow(rr10)){
  if (rr10[i,"hhw"]>59){
    if (rr10[i,"x01"] %in% ri2009[,"x01"]){
      row <- which(ri2009[,"x01"]==rr10[i,"x01"])
      if (ri2009[row,"gender"]=="male"){
        rr10[i,"hhw"] <- ri2009[row,"h02a"]
      } else {
        rr10[i,"hhw"] <- ri2009[row,"h02b"]
      }
    } else if (rr10[i,"x01"] %in% rr2009[,"x01"]){
      row <- which(rr2009[,"x01"]==rr10[i,"x01"])
      if (rr2009[row,"gender"]=="male"){
        rr10[i,"hhw"] <- rr2009[row,"c01a"]
      } else {
        rr10[i,"hhw"] <- rr2009[row,"c01b"]
      }
    }
  }
}
table(rr10[,"hhw"]<60)
#FALSE  TRUE 
#1    556

for (i in 1:nrow(rr10)){
  if (rr10[i,"w_wrkhrs"]>89){
    if (rr10[i,"x01"] %in% ri2009[,"x01"]){
      row <- which(ri2009[,"x01"]==rr10[i,"x01"])
      if (ri2009[row,"gender"]=="female"){
        rr10[i,"w_wrkhrs"] <- ri2009[row,"c07"]
      } else {
        rr10[i,"w_wrkhrs"] <- ri2009[row,"d22"]
      }
    } else if (rr10[i,"x01"] %in% rr2009[,"x01"]){
      row <- which(rr2009[,"x01"]==rr10[i,"x01"])
      if (rr2009[row,"gender"]=="female"){
        rr10[i,"w_wrkhrs"] <- rr2009[row,"c01a"]
      } else {
        rr10[i,"w_wrkhrs"] <- rr2009[row,"c01b"]
      }
    }
  }
}
table(rr10[,"w_wrkhrs"]<90)
#TRUE 
#557 

for (i in 1:nrow(rr10)){
  if (rr10[i,"h_wrkhrs"]>89){
    if (rr10[i,"x01"] %in% ri2009[,"x01"]){
      row <- which(ri2009[,"x01"]==rr10[i,"x01"])
      if (ri2009[row,"gender"]=="male"){
        rr10[i,"h_wrkhrs"] <- ri2009[row,"c07"]
      } else {
        rr10[i,"h_wrkhrs"] <- ri2009[row,"d22"]
      }
    } else if (rr10[i,"x01"] %in% rr2009[,"x01"]){
      row <- which(rr2009[,"x01"]==rr10[i,"x01"])
      if (rr2009[row,"gender"]=="male"){
        rr10[i,"h_wrkhrs"] <- rr2009[row,"c01a"]
      } else {
        rr10[i,"h_wrkhrs"] <- rr2009[row,"c01b"]
      }
    }
  }
}
table(rr10[,"h_wrkhrs"]<90)
#FALSE  TRUE 
#3   554 

rm(i,row)

# Finally, remove any that still have high values.

rr10 <- rr10[rr10[,"whw"]<60 & rr10[,"hhw"]<60 & rr10[,"w_wrkhrs"]<90 & rr10[,"h_wrkhrs"]<90,]

# rr11
rr11 <- data.frame(x01=rr2011[,"x01"],syear=2011,
                   weduH=weduH(rr2011,"a03c","a18"),
                   heduH=heduH(rr2011,"a03c","a18"),whw=whw(rr2011,"c01a","c01b"),
                   hhw=hhw(rr2011,"c01a","c01b"),
                   w_wrkhrs=w_wrkhrs(rr2011,"a11a","a26a"),h_wrkhrs=h_wrkhrs(rr2011,"a11a","a26a"),
                   parentsb=parentsb(rr2011,"b04b01","b04b02","b04b04","b04b05"),
                   w_inc=w_inc(rr2011,"a10a","a10b","a25a","a25b"),
                   h_inc=h_inc(rr2011,"a10a","a10b","a25a","a25b"),
                   gender=gender(rr2011),nanb=nanb(rr2011,"c08c"),
                   morebirb=morebirb(rr2011,"e03"),
                   tdum=integer(nrow(rr2011)),ddum=integer(nrow(rr2011))+1)
rr11 <- data.frame(rr11,genideo=genideo_lookup(rr11))
rr11 <- rr11[rr11[,"x01"] %in% kyobs[,"x01"],]

# Remove all observations that aren't in rr10.

rr11 <- subset.data.frame(rr11,rr11[,"x01"] %in% rr10[,"x01"])

birth <- integer(nrow(rr11))
for (i in 1:nrow(rr11)){
  row <- which(kyobs[,"x01"] == rr11[i,"x01"])
  birth[i] <- kyobs[row,"b2012"]
}

numc <- integer(nrow(rr11))
for (i in 1:nrow(rr11)){
  row <- which(kyobs[,"x01"] == rr11[i,"x01"])
  numc[i] <- kyobs[row,"c2011"]
}

coldYOB <- integer(nrow(rr11))
for (i in 1:nrow(rr11)){
  row <- which(kyobs[,"x01"] == rr11[i,"x01"])
  coldYOB[i] <- kyobs[row,"coldYOB"]
}

rr11 <- data.frame(rr11,wy_age=wy_plus(rr10,rr11,"wy_age",1))

rr11 <- data.frame(rr11,birth,numc,coldYOB)
rm(birth,numc,coldYOB,i,row)

# Strange values in the same 4 columns; impute from 2010.

for (i in 1:nrow(rr11)){
  if (rr11[i,"whw"]>59){
    row <- which(rr10[,"x01"]==rr11[i,"x01"])
    rr11[i,"whw"] <- rr10[row,"whw"]
  }
}

for (i in 1:nrow(rr11)){
  if (rr11[i,"hhw"]>59){
    row <- which(rr10[,"x01"]==rr11[i,"x01"])
    rr11[i,"hhw"] <- rr10[row,"hhw"]
  }
}

for (i in 1:nrow(rr11)){
  if (rr11[i,"w_wrkhrs"]>89){
    row <- which(rr10[,"x01"]==rr11[i,"x01"])
    rr11[i,"w_wrkhrs"] <- rr10[row,"w_wrkhrs"]
  }
}

for (i in 1:nrow(rr11)){
  if (rr11[i,"h_wrkhrs"]>89){
    row <- which(rr10[,"x01"]==rr11[i,"x01"])
    rr11[i,"h_wrkhrs"] <- rr10[row,"h_wrkhrs"]
  }
}

rm(i,row)

# rr12
rr12 <- data.frame(x01=rr2012[,"x01"],syear=2012,
                   weduH=weduH(rr2012,"a03c","a18"),
                   heduH=heduH(rr2012,"a03c","a18"),whw=whw(rr2012,"d01a","d01b"),
                   hhw=hhw(rr2012,"d01a","d01b"),
                   w_wrkhrs=w_wrkhrs(rr2012,"a11a","a26a"),h_wrkhrs=h_wrkhrs(rr2012,"a11a","a26a"),
                   parentsb=parentsb(rr2012,"b04b01","b04b02","b04b04","b04b05"),
                   w_inc=w_inc(rr2012,"a10a","a10b","a25a","a25b"),
                   h_inc=h_inc(rr2012,"a10a","a10b","a25a","a25b"),
                   gender=gender(rr2012),nanb=nanb(rr2012,"d08c"),
                   morebirb=morebirb(rr2012,"f03"),
                   tdum=integer(nrow(rr2012)),ddum=integer(nrow(rr2012)))
rr12 <- data.frame(rr12,genideo=genideo_lookup(rr12))
rr12 <- rr12[rr12[,"x01"] %in% kyobs[,"x01"],]

# Remove all observations that aren't in rr11.

rr12 <- subset.data.frame(rr12,rr12[,"x01"] %in% rr11[,"x01"])

birth <- integer(nrow(rr12))
for (i in 1:nrow(rr12)){
  row <- which(kyobs[,"x01"] == rr12[i,"x01"])
  birth[i] <- kyobs[row,"b2013"]
}

numc <- integer(nrow(rr12))
for (i in 1:nrow(rr12)){
  row <- which(kyobs[,"x01"] == rr12[i,"x01"])
  numc[i] <- kyobs[row,"c2012"]
}

coldYOB <- integer(nrow(rr12))
for (i in 1:nrow(rr12)){
  row <- which(kyobs[,"x01"] == rr12[i,"x01"])
  coldYOB[i] <- kyobs[row,"coldYOB"]
}

rr12 <- data.frame(rr12,wy_age=wy_plus(rr11,rr12,"wy_age",1))

rr12 <- data.frame(rr12,birth,numc,coldYOB)
rm(birth,numc,coldYOB,i,row)

# Strange values in the same 4 columns; impute from 2011.

for (i in 1:nrow(rr12)){
  if (rr12[i,"whw"]>59){
    row <- which(rr11[,"x01"]==rr12[i,"x01"])
    rr12[i,"whw"] <- rr11[row,"whw"]
  }
}

for (i in 1:nrow(rr12)){
  if (rr12[i,"hhw"]>59){
    row <- which(rr11[,"x01"]==rr12[i,"x01"])
    rr12[i,"hhw"] <- rr11[row,"hhw"]
  }
}

for (i in 1:nrow(rr12)){
  if (rr12[i,"w_wrkhrs"]>89){
    row <- which(rr11[,"x01"]==rr12[i,"x01"])
    rr12[i,"w_wrkhrs"] <- rr11[row,"w_wrkhrs"]
  }
}

for (i in 1:nrow(rr12)){
  if (rr12[i,"h_wrkhrs"]>89){
    row <- which(rr11[,"x01"]==rr12[i,"x01"])
    rr12[i,"h_wrkhrs"] <- rr11[row,"h_wrkhrs"]
  }
}

rm(i,row)

# rr14
rr14 <- data.frame(x01=rr2014[,"x01"],syear=2014,
                   weduH=weduH(rr2014,"a03c","a27"),
                   heduH=heduH(rr2014,"a03c","a27"),whw=whw(rr2014,"c01a","c01b"),
                   hhw=hhw(rr2014,"c01a","c01b"),
                   w_wrkhrs=w_wrkhrs(rr2014,"a09b","a33b"),h_wrkhrs=h_wrkhrs(rr2014,"a09b","a33b"),
                   parentsb=parentsb(rr2014,"b11b01","b11b02","b11b04","b11b05"),
                   w_inc=w_inc(rr2014,"a08a","a08b","a32a","a32b"),
                   h_inc=h_inc(rr2014,"a08a","a08b","a32a","a32b"),
                   gender=gender(rr2014),nanb=nanb(rr2014,"c08b"),
                   morebirb=morebirb(rr2014,"d03"),
                   tdum=integer(nrow(rr2014)),ddum=integer(nrow(rr2014)))
rr14 <- data.frame(rr14,genideo=genideo_lookup(rr14))
rr14 <- rr14[rr14[,"x01"] %in% kyobs[,"x01"],]

# Remove all observations that aren't in rr12.

rr14 <- subset.data.frame(rr14,rr14[,"x01"] %in% rr12[,"x01"])

birth <- integer(nrow(rr14))
for (i in 1:nrow(rr14)){
  row <- which(kyobs[,"x01"] == rr14[i,"x01"])
  birth[i] <- kyobs[row,"b2015"]
}

numc <- integer(nrow(rr14))
for (i in 1:nrow(rr14)){
  row <- which(kyobs[,"x01"] == rr14[i,"x01"])
  numc[i] <- kyobs[row,"c2014"]
}

coldYOB <- integer(nrow(rr14))
for (i in 1:nrow(rr14)){
  row <- which(kyobs[,"x01"] == rr14[i,"x01"])
  coldYOB[i] <- kyobs[row,"coldYOB"]
}

rr14 <- data.frame(rr14,wy_age=wy_plus(rr12,rr14,"wy_age",2))

rr14 <- data.frame(rr14,birth,numc,coldYOB)
rm(birth,numc,coldYOB,i,row)

# Some 996 values in whw, hhw, w_wrkhrs and h_wrkhrs. Otherwise, fine.
# Impute from rr2012.

for (i in 1:nrow(rr14)){
  if (rr14[i,"whw"]>59){
    row <- which(rr12[,"x01"]==rr14[i,"x01"])
    rr14[i,"whw"] <- rr12[row,"whw"]
  }
}

for (i in 1:nrow(rr14)){
  if (rr14[i,"hhw"]>59){
    row <- which(rr12[,"x01"]==rr14[i,"x01"])
    rr14[i,"hhw"] <- rr12[row,"hhw"]
  }
}

for (i in 1:nrow(rr14)){
  if (rr14[i,"w_wrkhrs"]>89){
    row <- which(rr12[,"x01"]==rr14[i,"x01"])
    rr14[i,"w_wrkhrs"] <- rr12[row,"w_wrkhrs"]
  }
}

for (i in 1:nrow(rr14)){
  if (rr14[i,"h_wrkhrs"]>89){
    row <- which(rr12[,"x01"]==rr14[i,"x01"])
    rr14[i,"h_wrkhrs"] <- rr12[row,"h_wrkhrs"]
  }
}

rm(i,row)

# The panel is now balanced.

mast <- rbind(rr10,rr11,rr12,rr14)
summary(mast)

# Can't subset after here: creates an unbalanced panel.
# Have to impute data instead.

# Make extra variables, for use in Stata. Parity 2 plus binary, wife work binary,
# household income, ratio of housework, household income squared, years since first birth,
# and age of youngest child.

mast <- data.frame(mast,p2plusb=integer(nrow(mast)),w_wrkb=integer(nrow(mast)),
                   inc=mast[,"w_inc"]+mast[,"h_inc"],incsq=(mast[,"w_inc"]+mast[,"h_inc"])^2,
                   ratio=integer(nrow(mast)),yb1=mast[,"syear"]-mast[,"coldYOB"],
                   cyng_age=integer(nrow(mast)))

for (i in 1:nrow(mast)){
  if (mast[i,"numc"]>1){
    mast[i,"p2plusb"] <- 1
  }
}

for (i in 1:nrow(mast)){
  if (mast[i,"w_wrkhrs"]>0){
    mast[i,"w_wrkb"] <- 1
  }
}

for (i in 1:nrow(mast)){
  if (mast[i,"whw"]==0 & mast[i,"hhw"]==0){
    mast[i,"ratio"] <- 0.5
  } else {
    mast[i,"ratio"] <- mast[i,"hhw"] / (mast[i,"hhw"] + mast[i,"whw"])
  }
}

for (i in 1:nrow(mast)){
  row <- which(kyobs[,"x01"]==mast[i,"x01"])
  if (mast[i,"syear"]==2010){
    mast[i,"cyng_age"] <- kyobs[row,"cyoung10YOB"]
  } else if (mast[i,"syear"]==2011){
    mast[i,"cyng_age"] <- kyobs[row,"cyoung11YOB"]
  } else if (mast[i,"syear"]==2012){
    mast[i,"cyng_age"] <- kyobs[row,"cyoung12YOB"]
  } else if (mast[i,"syear"]==2014){
    mast[i,"cyng_age"] <- kyobs[row,"cyoung14YOB"]
  }
}

rm(i)

# finally, coerce household incomes of zero to one, so that when we take log(householdincome)
# in the analysis in stata, there are no errors.

zero_inc_rows <- which(mast$inc == 0)
for (i in zero_inc_rows){
  mast[i,"inc"] <- 1
}
rm(zero_inc_rows,i)

# Write .dat files

write.dta(mast,"mast_w2014_dp_f2010.dta")

# b1115b_dp.dta file

t <- mast[mast[,"syear"]==2010,]
reg2010frame <- data.frame(t,b11_15b=integer(nrow(t)),cyoung10YOB=integer(nrow(t)))

for (i in 1:nrow(reg2010frame)){
  row <- which(k11_15b[,"x01"]==reg2010frame[i,"x01"])
  if (k11_15b[row,"yh"]==1){
    reg2010frame[i,"b11_15b"] <- 1
  }
}

for (i in 1:nrow(reg2010frame)){
  row <- which(kyobs[,"x01"]==reg2010frame[i,"x01"])
  reg2010frame[i,"cyoung10YOB"] <- kyobs[row,"cyoung10YOB"]
}

rm(i,t)

write.dta(reg2010frame,"b1115b_dp.dta")
save(reg2010frame,file="from2010_2010")


