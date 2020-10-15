library(cppRouting)
library(e1071)
library(optimbase)

##########################

#list of transactions taken from input, each transactions has id, source, destination, value and times t0, which states in which round
#the transaction should be initialized and tEnd - last round in which transaction must end.

listOfTransactions <- data.frame(id=integer(), source=integer(), destination=integer(), value=integer(),
                                 t0=integer(), tEnd=integer())
currentTransactions<-data.frame(listOfTransactions, parent=integer(),waiting= logical(),realized = numeric(),
                             arity = integer(), Id0 = numeric(), done = logical())
realizedTransactions <- data.frame(listOfTransactions, vmax=numeric())

#parameters used in simulation

nrounds <- 100
max_length <- 50

#graphThroughput  is a graph of all channels with throughput
#locked is matrix of temporarily blocked channels


locked=matrix(0,nrow = nrow(graphThroughput),ncol = nrow(graphThroughput))
locked_prev<-locked
T=0

#protocol is working in rounds

for (n in (1:nrounds)){
  T=T+1
  currentTransactions<-addTrans2(listOfTransactions[listOfTransactions$t0==T,],0,lista=currentTransactions)
  temporaryTransactions <- currentTransactions[currentTransactions$t0<=T & currentTransactions$done == 0 & currentTransactions$tEnd>T& (((currentTransactions$value-currentTransactions$blocked-currentTransactions$realized)>0.001)|currentTransactions$blocked<0.001),]
  temporaryTransactions <- rbind(temporaryTransactions,currentTransactions[currentTransactions$tEnd==T & currentTransactions$done == 0,])
  while (nrow(temporaryTransactions)==0) {
    T=T+1
    currentTransactions<-addTrans2(listOfTransactions[listOfTransactions$t0==T,],0,lista=currentTransactions)
    temporaryTransactions <- currentTransactions[currentTransactions$t0<=T & currentTransactions$done == 0 & currentTransactions$tEnd>=T& (((currentTransactions$value-currentTransactions$blocked-currentTransactions$realized)>0.001)|currentTransactions$blocked<0.001),]
  }
  if(all.equal(locked_prev,locked)!=1){ #creating structure needed for Dijkstra algorithm 
    g2 <- (graph1*(graph1>locked))
    graphThroughput <- graphThroughput*g2
    edges=ee(g2)
    directed_graph<-makegraph(edges,directed=TRUE)
    nodes<-unique(c(edges$from_vertex,edges$to_vertex))
    locked_prev<-locked}
    for (i in 1:nrow(temporaryTransactions)){#
      {id <- temporaryTransactions[i,]$id
      value <- temporaryTransactions[i,]$value
      source <- temporaryTransactions[i,]$source
      destination <- temporaryTransactions[i,]$destination
      parent <- temporaryTransactions[i,]$parent
      realized <- temporaryTransactions[i,]$realized
      t0 <-temporaryTransactions[i,]$t0
      tEnd <- temporaryTransactions[i,]$tEnd
      id0 <- temporaryTransactions[i,]$id0
      blocked <- temporaryTransactions[i,]$blocked
      temp_directed_graph <- directed_graph} 
      
      if(source == destination){
        currentTransactions[parent,]$realized <- value + currentTransactions[parent,]$realized
        currentTransactions[parent,]$blocked <- currentTransactions[parent,]$blocked - value
        currentTransactions[id,]$done <- 1
        currentTransactions[id,]$realized <- value
        } else if(realized == value){
        if(parent==0){
          realizedTransactions <- addTrans3(temporaryTransactions[i,c(7,2,3,4,5,6,4)],realizedTransactions)
          currentTransactions[id,]$done <- 1
          } else {
          currentTransactions[parent,]$realized <- realized + currentTransactions[parent,]$realized
          currentTransactions[parent,]$blocked <- currentTransactions[parent,]$blocked - realized
          currentTransactions[id,]$done <- 1
          }
      } else if(temporaryTransactions[i,]$tEnd == T){
        graphThroughput <- updateChannel(graphThroughput,source,destination,realized-value)
        if(parent==0){
          realizedTransactions <- addTrans3(temporaryTransactions[i,c(7,2,3,10,5,6,4)],realizedTransactions)
          currentTransactions[id,]$done <- 1
        } else {
          currentTransactions[parent,]$realized <- realized + currentTransactions[parent,]$realized
          currentTransactions[parent,]$blocked <- currentTransactions[parent,]$blocked - value
          currentTransactions[id,]$done <- 1
        }
      } else if ((source %in% nodes) != 1|(destination %in% nodes) != 1){
        
        currentTransactions[parent,]$blocked <- currentTransactions[parent,]$blocked - (blocked+realized)
        locked[currentTransactions[parent,]$source,source] <- 1
        } else if (is.na(get_distance_pair(Graph=directed_graph,from = source,
                                       to = destination))==1){
        currentTransactions[parent,]$blocked <- currentTransactions[parent,]$blocked - (blocked+realized)
        locked[currentTransactions[parent,]$source,source] <- 11
      } else { len_min_path = length(get_path_pair(Graph=directed_graph,from = source,
                                                   to = destination)[[1]])
      #list of available paths with different first intermediary
      values_paths=numeric()
      neigh_paths <- numeric()
      j=1
      pp = get_path_pair(Graph=directed_graph,from = source,
                         to = destination)[[1]]
      neigh <-as.numeric(pp[length(pp)-1])
	  #searching for through all different paths to recipient, that have length smaller than max_length
      if(length(pp)<max_length){ 
        while(length(pp)==len_min_path){
          neigh <-as.numeric(pp[length(pp)-1])
          if(graphThroughput[source,neigh]==0){
            locked[source,neigh]=1
          } else {values_paths[j] <- graphThroughput[source, neigh]
          neigh_paths[j] <- neigh
          j=j+1
          }
          temp_directed_graph$data[(temp_directed_graph$data$from==(temp_directed_graph$dict[temp_directed_graph$dict$ref==source,]$id)) & temp_directed_graph$data$to==(temp_directed_graph$dict[temp_directed_graph$dict$ref==neigh,]$id),]<-c(0,0,0)
          temp_nodes<-unique(c(temp_edges$from_vertex,temp_edges$to_vertex))
          if((source %in% temp_nodes) != 1|(destination %in% temp_nodes) != 1){
            length(pp) <- 100  
          } else if(is.na(get_distance_pair(Graph=temp_directed_graph,from = source,
                                            to = destination))!=1){
            pp = get_path_pair(Graph=temp_directed_graph,from = source,
                               to = destination)[[1]]  
          } else {length(pp) <- 100}          
        }
      }
      #distribution in channels
      if(j==1){#there is no available path
        if(parent==0){
          #nothing happens
        } else { #get back one step along the path
          currentTransactions[parent,]$arity <- currentTransactions[parent,]$arity - 1
          currentTransactions[parent,]$blocked <- currentTransactions[parent,]$blocked - (realized+blocked)
          currentTransactions[id,]$done <- 1
          locked[source,neigh]=1
        }
      } else {
        #capacities are greater than value
        if(sum(values_paths)>=value-(blocked+realized)){
          for(i in 1:length(neigh_paths)){
            currentTransactions <- addTrans3(neigh_paths[i],destination,(values_paths[i]/sum(values_paths))*(value-(blocked+realized)),
                                          T+1, tEnd-1, id0, id, currentTransactions)
            graphThroughput <- updateChannel(graphThroughput,source,neigh_paths[i],(values_paths[i]/sum(values_paths))*(value-(blocked+realized)))
          }
        } else {
          #capacities are smaller than transaction value
          for(k in 1:length(neigh_paths)){
            currentTransactions <- addTrans3(neigh_paths[k],destination,values_paths[k],
                                          T+1, tEnd-1, id0, id, currentTransactions)
            graphThroughput <- updateChannel(graphThroughput,source,neigh_paths[k],values_paths[k])
            locked[source, neigh_paths[k]] <- 1
          }
        }
        currentTransactions[id,]$arity <- currentTransactions[id,]$arity + j - 1
        currentTransactions[id,]$blocked <- currentTransactions[id,]$blocked + min((value-(blocked+realized)), sum(values_paths))
       }
    }
  }
}
