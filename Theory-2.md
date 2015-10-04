<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  TeX: { equationNumbers: { autoNumber: "AMS" } }
});
</script>
<script type="text/javascript"
  src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

# Bayesian Generalized Rating Curves
Birgir Hrafnkelsson$^{1}$,Solvi Rognvaldsson$^{1}$,Axel Orn Jansson$^{1}$ Helgi Sigurdarson$^{2}$ and Sigurdur M. Gardarsson$^{3}$   
$^{1}$ Faculty of Physical Sciences, School of Engineering and Natural Sciences, The University of Iceland, Iceland $^{2}$ Faculty of Industrial Engineering, Mechanical Engineering and Computer Science, School of Engineering and Natural Sciences, The University of Iceland, Iceland   
$^{3}$ Faculty of Environmental and Civil Engineering, School of Engineering and Natural Sciences, The University of Iceland, Iceland 


   
A rating curve is a model that describes the relationship between water stage and discharge in a river. The rating curve is estimated from paired observations of stage and discharge. The rating curve is used to predict discharge given stage. This is the main practical usage of rating curves as stage is substantially easier to directly observe than discharge. Two types of rating curves are implemented here. The first type is the power-law model which is commonly assumed in hydraulic practice. It is given by

$$Q=a(h-c)^b$$ (1)

where $Q$ is discharge, $h$ is stage, $a$, $b$ and $c$ are unknown constants, see e.g., Venetis (1970), Clarke (1999) and Clarke et al. (2000).
The second type of rating curves is the generalized power-law model. Its construction is based on the hydraulics of open channel flow given the formulas of Chézy and Manning which are of the form

$$Q = KR^{x}AS^{1/2}$$ (2)

where $K$ is a constant, $R$ is the hydraulic radius, $x$ is constant, $A$ is the cross section area, $S$ is the slope of the channel, further, $R = A/P$ where $P$ is the wetted perimeter. According to Chezy $x = 1/2$ while Manning claimed that $x = 2/3$. The form of the generalised rating curve is

$$Q = a(h-c)^{f(h)}$$ (3)

where a and c are constants and $f(h)$ is a function of h and referred to as the generalised power-law exponent. The relationship between (2) and (3) can be found by equating these two equations. In particular, the form of $f(h)$ can be derived, namely,

$$f(h) = \frac{(x+1)\{ \log A(h) - \log A(1) \}  - x \{ \log P(h) - \log P(1)  \}}{\log h}$$ (3)

Thus, $f(h)$ is a function of the constant $x$, the cross section $A$ and the wetted perimeter $P$.
The estimation approach is proposed for the estimation of the power-law model and the generalised power-law model. Bayesian inference requires specification of prior den- sities for unknown parameters and unknown quantities along with a full probabilistic specification of the observed data. Bayesian inference is based on the posterior density and summary statistics such as the posterior mean and 95% posterior intervals are based on the posterior density. Analytical formulas for these summary statistics are intractable in most cases and thus they are computed by generating samples from the posterior density using Markov chain Monte Carlo simulation. The Bayesian power-law model is presented on a logarithmic scale,

$$\log Q_i = \log a + b \log(h_i -c) + \epsilon_i, \quad i = 1,...,n,$$ (5)


where $\epsilon_i$ follows a normal distribution with mean zero and variance $\sigma_{\epsilon}^2$, $n$ is the number of paired observations and $a$, $b$ and $c$ are as before. The Bayesian inference scheme 2 implemented for the power-law model is standard, however, for efficient posterior simulation, first, samples from the joint marginal posterior density of $(c, \sigma_{\epsilon}^2)$ are obtained, then samples from the conditional posterior density of $(a, b)$ conditioned on $(c, \sigma_{\epsilon}^2)$ are obtained.
The Bayesian generalized power-law model is presented as Bayesian hierarchical model. The function $f(h)$ is modelled at the latent level as b plus a mean zero continuous stochastic process which is assumed to be twice differentiable. The model is presented on a logarithmic scale,

$$log Q_i = \log a + (b + \beta(h_i)) \log(h_i -c) + \varepsilon_i, \quad i = 1,...,n,$$ (6)

where $\epsilon_i$ follows a normal distribution with mean zero and variance $\sigma(h_i)_{\epsilon}^2$ that can vary with stage. Here the parameters $a$, $b$ and $c$ play a similar role as in the Bayesian power law model. The stochastic process $\beta(h)$ is assumed a priori to be a Gaussian process governed by a Matern covariance function with smoothness parameter $\nu = 2.5$, see Matern (1960), and it is constrained such that $\sum_{i=1}^n\beta(h_i)=0$. An efficient posterior simulation is achieved by sampling from the joint posterior density of the hyperparameters of the model, and then sampling from the conditional density the latent parameters conditioned on the hyperparameters.

