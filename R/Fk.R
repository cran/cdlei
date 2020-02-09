Fk <-
function(age, k){
  Fk <- vector(length = length(age))
  Fk[1] <- k
  for (i in 2:length(age)){
    Fk[i] <- ((1-k)^age[i])*k + Fk[i-1]  
  }
  return(Fk = Fk)
}
