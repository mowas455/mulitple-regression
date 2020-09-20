library(ggplot2) 
get_plot<-function(func){
  data = data.frame(cbind(func$residuals, func$fitted.vals))
  p=ggplot(data)+geom_point(aes(x=func$fitted.vals,y=func$residuals))
  x_label = as.character(paste0("Fitted Values\n",deparse(func$formula)))
  labels =labs(x=x_label,y="Residuals")
  title = ggtitle("Residuals Vs Fitted")
  plot(p+labels+title )
}


lin_data = linreg(Petal.Length~Species, data = iris)
print(lin_data$formula)

get_plot(lin_data)
