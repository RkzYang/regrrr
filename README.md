# regrrr (Toolkit for Compiling and Plotting Regression Results)
[![Rdoc](http://www.rdocumentation.org/badges/version/regrrr)](http://www.rdocumentation.org/packages/regrrr) 

# Description
Compiling regression results into a publishable format, conducting post-hoc hypothesis testing, and plotting moderating effects (the effect of X on Y becomes stronger/weaker as Z increases).

# Installation
To install from CRAN:
```
install.packages("regrrr")
library(regrrr)
```
You can also use devtools to install the latest development version:
```
devtools::install_github("raykyang/regrrr")
library(regrrr)
```

# Examples 
```
# build regression models using mtcars dataset
data(mtcars)
m0 <- lm(mpg ~ vs + carb + hp + wt, data = mtcars)
m1 <- update(m0, . ~ . + wt * hp)
m2 <- update(m1, . ~ . + wt * vs)
```

```
# compile the correlation table
cor.table(data = m2$model)
```

```
# compile the regression table
regression_table <- rbind(
combine_long_tab(to_long_tab(summary(m0)$coef),
                 to_long_tab(summary(m1)$coef),
                 to_long_tab(summary(m2)$coef)),
compare_models(m0, m1, m2))
rownames(regression_table) <- NULL
print(regression_table)
```

```
# plot the moderating effect
plot_effect(reg.coef = summary(m2)$coefficients, data = mtcars, model = m2,
            x_var.name = "wt", y_var.name = "mpg", moderator.name = "hp",
            confidence_interval = TRUE,  CI_Ribbon = FALSE, 
            xlab = "Weight", ylab = "MPG", moderator.lab = "Horsepower") +
ggplot2::theme(text=ggplot2::element_text(family="Times New Roman", size = 16))
```
