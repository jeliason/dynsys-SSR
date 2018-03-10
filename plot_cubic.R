plot_cubic<-function(df,times,var=1){
  if (var==1){
  plot(df[,3],df[,1],xlab="x",ylab="xdot")
  plot(times,df[,3],xlab="time",ylab="x")
  }
  else{
    plot(df[,4],df[,2],xlab="y",ylab="ydot")
    plot(times,df[,4],xlab="time",ylab="y")
  }
}