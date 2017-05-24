#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build model using train()
#~~~~~~~~~~~~~~~~~~~~~~~~~~
require(ggplot2)
require(caret)
load(mtcars)
mtcarss <- as.data.frame(mtcars)

model.mtcars_lm <- train(mpg ~ wt
                         ,mtcarss
                         ,method = "lm"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# Retrieve coefficients for
#  - slope
#  - intercept
#~~~~~~~~~~~~~~~~~~~~~~~~~~

coef.icept <- coef(model.mtcars_lm$finalModel)[1]
coef.slope <- coef(model.mtcars_lm$finalModel)[2]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot scatterplot and regression line
#  using ggplot()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  geom_abline(slope = coef.slope, intercept = coef.icept, color = "red")

