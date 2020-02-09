cdlei <-
function(age, qtau, qhiv, k, d){

df <- data.frame(age = age, qtau = qtau, qhiv = qhiv)
qx <- df$qtau/d
px <- 1 - qx

tpx <- vector(length = nrow(df))
tpx[1] <- px[1]

for (i in 2:nrow(df)){
  tpx[i] <- px[i]*tpx[i-1]  
}

sumtpx <- vector(length = nrow(df))
sumtpx[1] <- tpx[1]
for (i in 2:nrow(df)){
  sumtpx[i] <- tpx[i] + sumtpx[i-1]  
}


Fk <- vector(length = nrow(df))
Fk[1] <- k
for (i in 2:nrow(df)){
  Fk[i] <- ((1-k)^df$age[i])*k + Fk[i-1]  
}

pxx <- vector(length = nrow(df))
for (i in 1:nrow(df)){
  pxx[i] <- px[i] + (Fk[i]*df$qhiv[i]/d)  
}

tpxx <- vector(length = nrow(df))
tpxx[1] <- pxx[1]
for (i in 2:nrow(df)){
  tpxx[i] <- tpxx[i-1]*pxx[i]  
}

sumtpxx <- vector(length = nrow(df))
sumtpxx[1] <- tpxx[1]
for (i in 2:nrow(df)){
  sumtpxx[i] <- sumtpxx[i-1] + tpxx[i]  
}

#result
cdlei <- vector(length = nrow(df))
cdlei[1] <- sumtpxx[nrow(df)] - sumtpx[nrow(df)]
for (i in 2:nrow(df)){
  cdlei[i] <- sumtpxx[nrow(df)] - sumtpxx[i-1] - sumtpx[nrow(df)] + sumtpx[i-1]
}

return(list(cdlei = cdlei, qx = qx, px = px, tpx = tpx, sumtpx = sumtpx,
       Fk = Fk, pxx = pxx, tpxx = tpxx, sumtpxx = sumtpxx, df = df))
}
