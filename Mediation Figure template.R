library(diagram)
data <- c(0, "''", 0,0,
          0, 0, 0,0,
          "''", "''", 0,"''",
          0, "''", 0,0)
M <- matrix (nrow=4, ncol=4, byrow = TRUE, data=data)
plot<- plotmat (M, pos=c(1,2,1), 
                name= c( "2","1", "4", "3"), 
                box.type = "rect", box.size = 0.09, box.prop=0.5,  curve=0, shadow.size = 0)