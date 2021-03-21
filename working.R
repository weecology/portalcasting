#
#  have a series of jags models of increasing complexity all parameterized
#   using the same terms etc
#
#    1. mean overall density, not fluctuating
#    2. random walk overall density
#    3. logistic growth (r and K)
#    4. generalized ricker (r and K and a)
#
#
#  model generally works, has some understandable fitting issues with cyclics
#    the parameters are time invariant in the base model from lima
#    somehow the predicted value lines up way too nicely for PP...
#
#  but it works for all the needed species!

#
#  to do
#
#    add predictions beyond, etc.
#    incorporate set of models into proper function format for portalcasting


#
#  we should have some discussions about the priors and how we want to use
#   the data and interpret the model
#
#    what's an approprtiate prior for K? for a?
#
#  we also need to incorporate fluctuating dynamics --- which part of the 
#   model?
#

#
# computational concern
#
#  can we get it to throw an error and stop when it hangs instead of hanging?
#

