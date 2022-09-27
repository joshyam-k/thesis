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

First we often assume that $\beta_0, \beta_1, \sigma$ are independent this allows us to specify individual priors and then simply multiply their PDF's together.

Since $\beta_0, \beta_1$ can take any value on the real number line their priors should have a non-zero plausibilty across the entire real line. We can assume normal priors for both of these priors

$$
\beta_0 \sim \mathcal{N}(m_0,s_0^2) \qquad and \qqyad \beta_1 \sim \mathcal{N}(m_1, s_1^2)
$$

where we can tune the hyperparameters based on our prior understanding of $\beta_0, \beta_1$

Since the standard deviation parameter $\sigma$ must be positive we can we can assume an exponential model for its prior.

$$
\sigma \sim \text{Exp}(l)
$$

### Posterior Estimation

Assume we have $y = (y_1, ..., y_n)$ a collection of n observed values for $Y$.

Recall that our assumption that our parameters were independent means that our joint prior is just

$$
f(\beta_0, \beta_1, \sigma) = f(\beta_0)f(\beta_1)f(\sigma)
$$

Next, the likelihood function given the observed data $y$ is just the product of the marginal PDF's defined by the normal data structure that we assumed above. So we have:

$$
f(\beta_0, \beta_1, \sigma | y) \propto \bigg[\prod_{i=1}^nf(y_i |\beta_0, \beta_1, \sigma)\bigg]f(\beta_0)f(\beta_1)f(\sigma)
$$











