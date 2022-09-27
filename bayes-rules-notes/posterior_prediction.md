## Simple Linear Regression

Assume we have data on Basal Area (Y) and Tree Canopy Cover (X) indexed over some level $i$. We might assume that each $Y_i$ is normally distributed around some mean $\mu$ and standard deviation $\sigma$. Assume we're interested in the local mean for each index $i$. We can assume the following simple linear model:

$$
\mu_i = \beta_0 + \beta_1 X_i
$$

We can place this assumption about the linear relationship between $\mu_i$ and $X_i$ right into the idea that $Y_i$ is normally distributed by placing this assumption right into our bayesian set up:

$$
Y_i | \beta_0, \beta_1, \sigma \sim \mathcal{N}(\mu_i, \sigma^2) \qquad where \qquad \mu_i = \beta_0 + \beta_1 X_i
$$

### Specifying Priors

