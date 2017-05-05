# Prediction using logistic regression 

library(ISLR)
summary(Smarket)
str(Smarket)
cor(Smarket[,-9])

# glm model 
glm.model = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family=binomial)
summary (glm.model)
# None of the independant variables seem significant (P values > 0.05)

glm.pred = predict(glm.model, type="response") 
glm.pred [1:10]

glm.pred1=rep("Down",1250) 
glm.pred1[glm.pred >.5]=" Up"

table(glm.pred1, Smarket$Direction) 
(457+141)/(507+145+457+141) # 48% accuracy 
mean(glm.pred1==Smarket$Direction) # 11% 
# The accuracy of the model not very good 

# Lets split the model into train and test to improve accuracy 
train=(Smarket$Year < 2005) 
Smarket.2005= Smarket [!train ,] 
dim(Smarket.2005)
Direction.2005= Smarket$Direction [!train]

# glm model 
glm.model = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                data=Smarket, family=binomial, subset=train) 
glm.predict1=predict(glm.model, Smarket.2005, type="response")

# Lets see how well the model performs now 
glm.pred2 = rep("Down",252)
glm.pred2[glm.probs >.5]=" Up"
table(glm.pred2 ,Direction.2005) 
(97+34)/(44+77+34+97) # 52% accuracy 
mean(glm.pred2!=Direction.2005) # 70% - Test error rate is not good 

# Lets try and improve the model by removing x variables which had the least significance 
glm.model = glm(Direction ~ Lag1 + Lag2,
                data=Smarket, family=binomial, subset=train) # new model 
summary(glm.model)
  glm.pred1=predict(glm.model, Smarket.2005, type="response") 
  glm.pred2=rep("Down",252) 
  glm.pred2[glm.pred1 >.5]=" Up" 
  table(glm.pred2, Direction.2005) 
  (76+35)/(76+35+106+35) # 44% 
  mean(glm.pred2==Direction.2005) # 14# 
  
# Example prediction 
predict (glm.model, newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")
#         1         2 
#    0.4791462 0.4960939 


# Conclusion: 
# The model doesnt have a very strong accuracy 
# This may be due to the fact that none of the predictors are significant (P values < 0.05)
# To imrpove the model, we could look at other independant variables which are statistically significant predictors 