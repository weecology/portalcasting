running through portalcast() with options_all = all_options(), noting here
what needs to be added to the options lists for flexibly hindcasting

metadata gets filled when all of the data get prepped
if it's just a normal forecast, things can go on


we might want to do a singular hindcast, tho
or whatever
in that case, we don't need to have the covariates get forecast for current
time
















thoughts and stuff down here may or may not still be useful

add "end" as a single value or vector to 
rodents, covariates, metadata in the
prep____ functions

if end is NULL, then it's the most recent moon, like always, and that gets 
filled in. 




if hindcasting, 
the rodent set up will be the same just with the data ending earlier
the covariate set up will be different





need to add more checks in to portalcast