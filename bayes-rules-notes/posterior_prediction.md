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
\beta_0 \sim \mathcal{N}(m_0,s_0^2) \qquad and \qquad \beta_1 \sim \mathcal{N}(m_1, s_1^2)
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

If we were to write out the actual form of the distributions and multiply them together we would not discover some familiar structure. Instead we employ MCMC methods to *approimate* the posterior here.

If we have 20,000 runs this means that our MCMC procedure will provide 20,000 posterior plausible pairs of $\beta_0$ and $\beta_1$ values. These pairs capture our overall uncertainty about this relationship.

### Posterior Prediction

At this point we might be tempted to extract the median of the marginal posterior distributions for $\beta_0$ and $\beta_1$ and use these values as the coefficients for our linear models but this ignores two important sources of variability
- Sampling variability: the observed outcomes $Y$, typically deviate from the model line, thus for a given value of $X_i$ we don't expect to have the same exact predicted outcome value
- Posterior variability: the posterior median model is simply the the center of the range of plausible values for the parameters.

The **posterior predictive model** for a new data point $Y_{new}$ accounts for both sources of variability. Specifically it weighs the chance of the outcome $Y_{new} = y_{new}$ under any set of possible parameters by the posterior plausability of those parameters:

$$
f(y_{new} | y) = \int \int \int f(y_{new}| \beta_0, \beta_1, \sigma)f(\beta_0, \beta_1, \sigma | y) d\beta_0 d\beta_1 d\sigma
$$


While we don't have a nice formula for $f(\beta_0, \beta_1, \sigma | y)$ we do have 20,000 sets of the form $(\beta_0^{(i)}, \beta_1^{(i)}, \sigma^{(i)})$ and so we can *approximate* the posterior predictive model for $Y_{new}$ at a given value of $X_i$ by simulating a prediction from normal model evaluated on each parameter set

$$
Y_{new}^{(i)} | \beta_0, \beta_1, \sigma \sim \mathcal{N}(\mu^{(i)}, (\sigma^{(i)})^2) \qquad where \qquad \mu^{(i)} = \beta_0^{(i)} + \beta_1^{(i)}X_i
$$

Suppose that for one of our MCMC sets we had $(-5, 20, 100)$ and suppose $X_i = 75$ then we can use the above formula to get that $\mu^{(i)} = -5 + 20*75 = 1495$ and we use this along with $\sigma^{(i)}$ to get our first official prediction $Y_{new}^{(i)}$ by drawing from

$$
Y_{new}^{(i)} | \beta_0, \beta_1, \sigma \sim \mathcal{N}(1495, 100^2)
$$

We can then do this 19,999 more times, once for each set from our MCMC and this is what will give us our posterior predictive model for that given $X_i$.

The beauty of this is that we now have a predictive *distribution* and so we can very easily compute things like standard errors and mean squared errors for individual predictions.



