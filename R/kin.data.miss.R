kin.data.miss <-
function(ntype, Ped){
kin.data1 = kin.data(ntype, Ped)
kin.miss=vector("list",length=length(ntype))
vect.cont.kin.miss = c(rep(0,length(ntype)))
miss.cont=0
for (i in 1:length(kin.data1)){
k=0  #### cet indice c'est pour supprimer bien les bonnes colonnes correspendantes a des NA
kin.data.miss1 = kin.data1[[i]]
for (j in 1:ntype[i]){
r=0
for (l in 1:dim(Ped)[2]) {
                            if (is.na(Ped[(miss.cont+j), l]) == TRUE)   r=r+1
                            }
if ( r > 0 ) { 
if (is.matrix(kin.data.miss1) == TRUE) {
                                       kin.data.miss1 = kin.data.miss1[-(j-k), -(j-k)]
                                       } else {
                                              if (is.vector(kin.data.miss1) == TRUE) {
                                                                                     vect.cont.kin.miss[i] = i
                                                                                     kin.data.miss1 = 0
                                                                                     }
                                              }
k=k+1
}
}

kin.miss[[i]] = kin.data.miss1
miss.cont = miss.cont + ntype[i]
}

oo <- vect.cont.kin.miss[which(vect.cont.kin.miss!=0)]
if (length(oo) == 0){
                    res = kin.miss
                    } else{ 
                          res = kin.miss[(-oo)] 
                          }

return(res)
}
