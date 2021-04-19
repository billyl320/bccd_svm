#computing average accuracy over all classes

#order is plate, WBC, RBC

#radial
rad<-c( (104/108),
        (108/(108+3)),
        (1244/(1244+1))
         )
mean(rad)

#sigmoid
sig<-c( (106/108),
        (102/(102+9)),
        (1243/(1243+1+1))
        )
mean(sig)

#poly
pol<-c( (105/108),
        (108/(108+3)),
        (1242/(1242+3))
        )
mean(pol)

poly_sp<-c( 97/(11+97),
        34/(34+77),
        1238/(1238+5+2)
      )

mean(poly_sp)

poly_ei<-c( 96/(12+96),
        105/(105+6),
        1237/(1236+6+2)
      )

mean(poly_ei)

lin<-c( 105/108,
        106/111,
        1242/(1242+2+1)
      )

mean(lin)

lin3_sp<-c( 105/108,
        64/(64+47),
        1236/(1236+6+3)
      )

mean(lin3_sp)

lin3_ei<-c( 105/108,
        106/(106+5),
        1237/(1236+5+3)
      )

mean(lin3_ei)


#to hold outperformance for each
vals<-c()

#comparing with others
plate<-c(pol[1]/.961,pol[1]/.730,
        pol[1]/.798,
        pol[1]/.878, pol[1]/0.742
        )

vals[1]<-mean(plate)

wbc<-c(pol[2]/.869,pol[2]/1.0,
        pol[2]/.951,
        pol[2]/1.0, pol[2]/0.934
        )

vals[2]<-mean(wbc)

rbc<-c(pol[3]/.964,pol[3]/.909,
        pol[3]/.873,
        pol[3]/.964, pol[3]/0.836
        )

vals[3]<-mean(rbc)

#mean outperformance
mean(vals-1)

plate<-c(lin3[1]/.961,lin3[1]/.730,
        lin3[1]/.798,
        lin3[1]/.878, lin3[1]/0.742
        )

vals[1]<-mean(plate)

wbc<-c(lin3[2]/.869,lin3[2]/1.0,
        lin3[2]/.951,
        lin3[2]/1.0, lin3[2]/0.934
        )

vals[2]<-mean(wbc)

rbc<-c(lin3[3]/.964,lin3[3]/.909,
        lin3[3]/.873,
        lin3[3]/.964, lin3[3]/0.836
        )

vals[3]<-mean(rbc)

#mean outperformance
mean(vals-1)

#
