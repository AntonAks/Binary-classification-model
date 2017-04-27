library(data.table)
library(ggplot2)
library(MASS)

# Load data ####
train_data <- fread("~/R/Binary-classification-model-Titanic/dataset/train.csv", stringsAsFactors = TRUE)
test_data <- fread("~/R/Binary-classification-model-Titanic/dataset/test.csv", stringsAsFactors = TRUE)

train_data[,type_data:=1]
test_data[,type_data:=0]


full_data <- rbindlist(l = list(train_data,test_data)
                       ,use.names = TRUE
                       ,fill = TRUE
                       ,idcol = NULL)


# NA detection ####
f.show_na_and_empty <- function(){
  
  na_values <- data.frame('NA_and_Empty_train' = apply(X = full_data[full_data$type == 1,],2, function(x){sum(is.na(x) | x == "")})
                          ,'NA_and_Empty_test' = apply(X = full_data[full_data$type == 0,],2, function(x){sum(is.na(x) | x == "")}))
  
  na_values <- na_values[na_values$NA_and_Empty_train>0 | na_values$NA_and_Empty_test>0,]
  
  return(na_values)    
}

# multiplot ####
f.multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


f.show_na_and_empty()


# Missing values ####
# AGE
p1 <- ggplot(data = full_data[!is.na(Age)], aes(x = Age, fill = factor(type_data))) + 
  geom_density (col = 'black', alpha = 0.3)


age_predict_model <- lm(Age ~ SibSp + Pclass + Sex + Parch, data = full_data[!is.na(Age)&!is.na(Fare)])
summary(age_predict_model)

full_data[is.na(Age),Age.pred:='pred']
full_data[is.na(Age),Age:=median(full_data[!is.na(Age),Age])]

p2 <- ggplot(data = full_data[], aes(x = Age, fill = factor(Age.pred))) + 
  geom_density (col = 'black', alpha = 0.3)


f.multiplot(p1,p2)

