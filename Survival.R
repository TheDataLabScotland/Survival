library( survival )
library( ggplot2 )

options( na.action = na.exclude ) # retain NA in predictions (???)
data( lung )
View( lung )

# Fit cox ph
#%%%%%%%%%%%%%
res.cox <- coxph( Surv( time, status ) ~ age + sex + wt.loss, data = lung )
summary( res.cox )

#lung data set has status coded as 1/2
mresid <- ( lung$status - 1 ) - predict( res.cox, type = 'expected' ) # Martingale resid 

predict( res.cox, type = "lp" )
predict( res.cox, type = "expected" )
predict( res.cox, type = "risk", se.fit = TRUE )
predict( res.cox, type = "terms", se.fit = TRUE )

surv_pred <- survfit( res.cox, newdata = lung )
plot( surv_pred )

lung.res <- lung
cbind( lung.res, summary(surv_pred)$table )

# https://www.kaggle.com/hj5992/bank-churn-modelling
