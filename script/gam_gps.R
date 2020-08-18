# setup
options(scipen=999)
library(mgcv)
library(gratia)
##################################################
test <- mgcv::gam(price ~ s(lat,lng), data = data_gps_sample_dublin, method = "REML")

gratia::draw(test)
vis.gam(test)
vis.gam(test,type="response", plot.type="contour")
vis.gam(test, view = c("lat","lng"), plot.type = "persp", se = 2)
vis.gam(test, view = c("lat","lng"), 
        plot.type = "contour", too.far = 0.05)

gratia::appraise(test, type = "response")
