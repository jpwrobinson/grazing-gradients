

library(betapart)
library(adespatial
     library(ade4)

     data(doubs)   # Data available in \code{"ade4"}
     fish.sp = doubs$fish[-8,]   
     # Fish data; site 8 is removed because no fish were caught
     # We use abundance data in this example, not presence-absence data
     
     # Partition into Baselga-family replacement and nestedness components 
     # using \code{"beta.div.comp"} with the percentage difference index (aka Bray-Curtis)
     out.comp = beta.div.comp(fish.sp, coef="BS", quant=TRUE)
     out.comp$part

     # Compute the D and component matrices using \code{"beta.pair.abund"}
     out3 = beta.pair.abund(fish.sp, index.family = "bray")
     summary(out3)
     
     is.euclid(out3$beta.bray)    # D matrix out3$beta.bray is not Euclidean
     out3.D = LCBD.comp(out3$beta.bray, sqrt.D=TRUE)
     out3.D$beta
     # Compare BDtotal here to BDtotal in out.comp$part (above)
     
     out3.Repl = LCBD.comp(out3$beta.bray.bal, sqrt.D=TRUE)
     out3.Repl$beta
     # Compare BDtotal here to RichDiff in out.comp$part (above)
     
     out3.AbDiff = LCBD.comp(out3$beta.bray.gra, sqrt.D=TRUE)
     out3.AbDiff$beta
     # Compare BDtotal here to RichDiff/Nes in out.comp$part (above)
     ## End(Not run)