
# test.R

#reforma_tributaria <- fread("~/Documentos/reforma_tributaria.csv", encoding = "Latin-1")

{
  #rm(list = ls())
  for (a in list.files("R/")){

    a <- paste0("R/", a)

    print(a)

    source(a)

    rm(a)

  }
  ClusterViz()
}
