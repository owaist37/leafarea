#Name: Owais Tariq
#Student ID: 10039105
library("lpSolveAPI")

#function to maximise the diagonal of a matrix in order to get the classification accuracy 
#created by using the example from the demo on a 6x6 matrix and then downsizing
#probably a nicer way to this than solving linear equations 
allignDiag = function(x){
  #x is a table, convert into matrix for use
  x = as.matrix(x)
  model = make.lp(10,25)
  add.constraint(model, c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0),"=",1)
  add.constraint(model, c(0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0),"=",1)
  add.constraint(model, c(0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0),"=",1)
  add.constraint(model, c(0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0),"=",1)
  add.constraint(model, c(0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1),"=",1)
  add.constraint(model, c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0),"=",1)
  add.constraint(model, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),"=",1)
  for (j in 1:25) {
    set.type(model, j, "binary")
  }
  set.objfn(model,as.vector(t(x)))
  lp.control(model,sense='max')
  solve(model)
  #get the accuracy
  accuracy = (get.objective(model)/sum(x))*100
  #get the alligned model
  orders = matrix(get.variables(model), nrow = 5,byrow = T)
  confusion_matrix=x[,which.max(orders[1,])]
  for (k in 2:5) {
    confusion_matrix = rbind(confusion_matrix,x[,which.max(orders[k,])])
  }
  confusion_matrix=t(confusion_matrix)
  return(list(confusion_matrix,accuracy))
}