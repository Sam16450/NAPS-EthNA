addTrans <- function(from, to, v, tstart, tfinal, parent, lista){
  df<-data.frame(nrow(lista)+1,from, to, v, tstart, tfinal, parent)
  names(df)<-c("id","source","destination","value","t0","tEnd", "parent")
  return(rbind(lista, df))
}

addTrans2 <- function(d,parent,lista){
  if(nrow(d)==0){
    return(lista)
  }
  d$id0=d$id
  d$id=c(1:nrow(d))+nrow(lista) 
  d$parent = parent
  d$blocked = 0 #coins blocked
  d$realized = 0 #blocked+realized +waiting= value
  d$waiting = 0
  d$arrity = 0
  d$done = 0
  names(d)<-c("id","source","destination","value","t0",
              "tEnd","id0", "parent", "blocked", "realized",
              "waiting", "arrity", "done")
  return(rbind(lista, d))
}

addTrans9 <- function(df,lista){
  names(df)<-c("id","source","destination","value","t0","tEnd","vmax")
  return(rbind(lista, df))
}

updateChannel <- function(g,i,j,value){
  gg <- g
  gg[i,j] <- gg[i,j] - value
  gg[j,i] <- gg[j,i] + value
  return(gg)
}

ee <- function(graph){
  k=1
  from = 1
  to=1
  cost=1
  for (i in 1:nrow(graph)){
    for (j in 1:ncol(graph)){
      if(graph[i,j]!=0 && is.na(graph[i,j])== FALSE){
        from[k]=i
        to[k]=j
        cost[k]=graph[i,j]
        k=k+1
      }
    }
  }
  return(edges<-data.frame(from_vertex=from,
                           to_vertex=to,
                           cost=cost))
}
