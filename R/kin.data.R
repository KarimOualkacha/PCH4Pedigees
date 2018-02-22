kin.data <-
function(ntype, Ped){
kin=vector("list",length=length(ntype))  ### voir la fonction precedente pour comprendre c'est quoi ntype

k=0
for (i in 1:length(ntype)){
if (ntype[i] == 1){
                  kin[[i]] = 0.5
                  k = k + ntype[i]
                  }  else{
                         id=Ped[(k + 1):(k + ntype[i]),2]
                         fid=Ped[(k + 1):(k + ntype[i]),3]
                         mid=Ped[(k + 1):(k + ntype[i]),4]
                         kin[[i]]=kinship(id,fid,mid)
                         k = k + ntype[i]
                         }
                          }
return(kin)
}
