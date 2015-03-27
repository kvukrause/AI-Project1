##Partition Analysis does not require every library listed here, however
##I did make use of each library at some point in development.
##The needed libraries are nnet, scatterplot3d,randomForest
library(MASS)
library(NeuralNetTools)
library(nnet)
library(neuralnet)
library(scatterplot3d)
library(rpart)
library(randomForest)
##This dataset is a custom combination of the Fragile States Index and the Global Terrorism Database in country-year units
AIMS <- AIMS.csv
attach(AIMS)
names(AIMS)

##Tree Analysis Used Here to Parse the Large Dataset (by finding the most important variables)
##In this case, rpartS refers to S.Actor Decisions
rpartS <- rpart(Legitimacy.of.the.State ~ Demographic.Pressures + Refugees.and.IDPs +
         Human.Flight + Uneven.Development + Group.Grievance + Poverty.and.Economic.Decline+
         Public.Services + Human.Rights + Security.Apparatus+Factionalized.Elites+
         External.Intervention,data=AIMS)
##The Most Valuable Variable is Listed Here As Public.Services with a Score of 22)
summary(rpartS)
plot(rpartT,uniform=T,branch=0.5,compress=T)
text(rpartS)
##Adds labels to the rpartS CART

##rpartT refers to T.Actor Decisions
rpartT <- rpart(Group.Grievance ~ Demographic.Pressures + Refugees.and.IDPs +
         Human.Flight + Legitimacy.of.the.State + Uneven.Development + Poverty.and.Economic.Decline+
         Public.Services + Human.Rights + Security.Apparatus+Factionalized.Elites+
         External.Intervention,data=AIMS)
summary(rpartT)
plot(rpartT,uniform=T,branch=0.5,compress=T)
text(rpartT)
##And rpartP refers to P.Actor Decisions
rpartP <- rpart(Refugees.and.IDPs ~ Demographic.Pressures + Group.Grievance + Legitimacy.of.the.State +
                  Human.Flight + Uneven.Development + Poverty.and.Economic.Decline+
                  Public.Services + Human.Rights + Security.Apparatus+Factionalized.Elites+
                  External.Intervention,data=AIMS)
summary(rpartP)
plot(rpartP,uniform=T,branch=0.5,compress=T)
text(rpartP)
## The scatterplot below presents a bimodal variable model, with a single outlying variable
DScat <- scatterplot3d(rpartS$variable.importance,rpartT$variable.importance,rpartP$variable.importance,
                       color=par("col"),pch=par("pch"),
                       main = "Variable Importance From Tree Analysis",
                       xlab = "S.Actor Best Worldview",
                       ylab = "T.Actor Best Worldview",
                       zlab = "P.Actor Best Worldview",
                       scale.y = 1:1, angle = 45,axis=T,
                       grid=T,box=T,type="h")
## The scatterplot created above also demonstrates that what appears to be a linear relationship between P.best
## & T.best & S.best variables;
## Hypothesize a threshold network model
##Step 1: Define Each input variable as a binomial quantity using <leaf>
Leaf.S <- rpartS$frame$var
Leaf.S <- rpartS$frame$var=="<leaf>"
Leaf.T <- rpartT$frame$var
Leaf.T <- rpartT$frame$var=="<leaf>"
Leaf.P <- rpartP$frame$var
Leaf.P <- rpartP$frame$var=="<leaf>"
print(Leaf.S)
print(Leaf.T)
print(Leaf.P)
traininginput <-  as.data.frame(Leaf.S)
trainingoutput <- as.data.frame(Leaf.T)

##Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

##Step 2: Train the neural network:
##Going to have 5 hidden layer nodes
##Threshold is a numeric value specifying the threshold for the partial
##derivatives of the error function as stopping criteria.
net.st <- neuralnet(Output~Input,trainingdata, hidden=5, threshold=0.00000001)
print(net.st)

##Plot the neural network
plot(net.st)
names(net.st)
##net.st results will be expressed as the partial derivative of the error function.
##This will become more clear later in the code.
print(net.st$net.result)

##Test the neural network on new data
i <- 0
net.st <- while(i < 500){  testdata <- sample(1:nrow(AIMS), 50,replace=TRUE)
                net.results <- compute(net.st, testdata)
                i = i+1
                }

##Raw results
plot(net.results$net.result)
print(net.results$net.result)


