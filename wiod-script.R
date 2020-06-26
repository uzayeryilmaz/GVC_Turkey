
fix_rows = function(wiot) {
  country_list = unique(wiot[,3])[-length(unique(wiot[,3]))]
  
  final = data.frame()
  
  for (cnt_n in 0:(length(country_list)-1)) {
    a = data.frame()
    c = data.frame()
    e = data.frame()
    g = data.frame()
    h = data.frame()
    j = data.frame()
    k = data.frame()
    m = data.frame()
    
    for (i in c(1:56)+cnt_n*56) {
      if(i %in% (c(1:3)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        a = rbind(a, asd)}
      
      if(i %in% (c(5:23)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        c = rbind(c, asd)}
      
      if(i %in% (c(25:26)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        e = rbind(e, asd)}
      
      if(i %in% (c(28:30)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        g = rbind(g, asd)}
      
      if(i %in% (c(31:35)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        h = rbind(h, asd)}
      
      if(i %in% (c(37:40)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        j = rbind(j, asd)}
      
      if(i %in% (c(41:43)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        k = rbind(k, asd)}
      
      if(i %in% (c(45:49)+cnt_n*56)) {
        asd = subset(wiot[i,], select=-c(IndustryCode,IndustryDescription,Country,RNr,Year))
        m = rbind(m, asd)}
      
      
      
      
    }
    A = data.frame(IndustryCode="A",
                   IndustryDescription="Agriculture, forestry and fishing",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(a)))
    B = wiot[4+cnt_n*56,]
    C = data.frame(IndustryCode="C",
                   IndustryDescription="Manufacturing",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(c)))
    D = wiot[24+cnt_n*56,]
    E = data.frame(IndustryCode="E",
                   IndustryDescription="Water supply; sewerage; waste managment and remediation activities",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(e)))
    F = wiot[27+cnt_n*56,]
    G = data.frame(IndustryCode="G",
                   IndustryDescription="Wholesale and retail trade; repair of motor vehicles and motorcycles",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(g)))
    H = data.frame(IndustryCode="H",
                   IndustryDescription="Transporting and storage",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(h)))
    I = wiot[36+cnt_n*56,]
    J = data.frame(IndustryCode="J",
                   IndustryDescription="Information and communication",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(j)))
    K = data.frame(IndustryCode="K",
                   IndustryDescription="Financial and insurance activities",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(k)))
    L = wiot[44+cnt_n*56,]
    M = data.frame(IndustryCode="M",
                   IndustryDescription="Professional, scientific and technical activities",
                   Country=country_list[cnt_n+1],
                   RNr=NA,
                   Year=wiot[1,"Year"],
                   t(colSums(m)))
    others = wiot[c(50:56) + cnt_n*56,]
    
    cnt_final = rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,others)
    final = rbind(final, cnt_final)
    
  }
  
  final = rbind(final , wiot[c(2465:2472),])

  return(final)

}


fix_cols = function(wiot) {
  country_list = unique(wiot[,3])[-length(unique(wiot[,3]))]
  final = data.frame()
  
  for (cnt_n in 0:(length(country_list)-1)) {
    a = data.frame()
    c = data.frame()
    e = data.frame()
    g = data.frame()
    h = data.frame()
    j = data.frame()
    k = data.frame()
    m = data.frame()
    
    for (i in c(1:56)+cnt_n*56) {
      
      if(i %in% (c(1:3)+cnt_n*56)) {
        if (length(a) == 0) {
          a = wiot[,i+5]
        }
        else {
          a = cbind(a, wiot[,i+5])}
      }
      
      
      if(i %in% (c(5:23)+cnt_n*56)) {
        if (length(c) == 0) {
          c = wiot[,i+5]
        }
        else {
          c = cbind(c, wiot[,i+5])}
      }
      
      if(i %in% (c(25:26)+cnt_n*56)) {
        if (length(e) == 0) {
          e = wiot[,i+5]
        }
        else {
          e = cbind(e, wiot[,i+5])}
      }
      
      if(i %in% (c(28:30)+cnt_n*56)) {
        if (length(g) == 0) {
          g = wiot[,i+5]
        }
        else {
          g = cbind(g, wiot[,i+5])}
      }
      
      if(i %in% (c(31:35)+cnt_n*56)) {
        if (length(h) == 0) {
          h = wiot[,i+5]
        }
        else {
          h = cbind(h, wiot[,i+5])}
      }
      
      if(i %in% (c(37:40)+cnt_n*56)) {
        if (length(j) == 0) {
          j = wiot[,i+5]
        }
        else {
          j = cbind(j, wiot[,i+5])}
      }
      
      if(i %in% (c(41:43)+cnt_n*56)) {
        if (length(k) == 0) {
          k = wiot[,i+5]
        }
        else {
          k = cbind(k, wiot[,i+5])}
      }
      
      if(i %in% (c(45:49)+cnt_n*56)) {
        if (length(m) == 0) {
          m = wiot[,i+5]
        }
        else {
          m = cbind(m, wiot[,i+5])}
      }
    }
    
    A = data.frame(rowSums(a))
    names(A) = paste(country_list[cnt_n+1],"A",sep = "_")
    B = data.frame(wiot[4+cnt_n*56+5])
    names(B) = paste(country_list[cnt_n+1],"B",sep = "_")
    C = data.frame(rowSums(c))
    names(C) = paste(country_list[cnt_n+1],"C",sep = "_")
    D = data.frame(wiot[,24+cnt_n*56+5])
    names(D) = paste(country_list[cnt_n+1],"D",sep = "_")
    E = data.frame(rowSums(e))
    names(E) = paste(country_list[cnt_n+1],"E",sep = "_")
    F = data.frame(wiot[,27+cnt_n*56+5])
    names(F) = paste(country_list[cnt_n+1],"F",sep = "_")
    G = data.frame(rowSums(g))
    names(G) = paste(country_list[cnt_n+1],"G",sep = "_")
    H = data.frame(rowSums(h))
    names(H) = paste(country_list[cnt_n+1],"H",sep = "_")
    I = data.frame(wiot[,36+cnt_n*56+5])
    names(I) = paste(country_list[cnt_n+1],"I",sep = "_")
    J = data.frame(rowSums(j))
    names(J) = paste(country_list[cnt_n+1],"J",sep = "_")
    K = data.frame(rowSums(k))
    names(K) = paste(country_list[cnt_n+1],"K",sep = "_")
    L = data.frame(wiot[,44+cnt_n*56+5])
    names(L) = paste(country_list[cnt_n+1],"L",sep = "_")
    M = data.frame(rowSums(m))
    names(M) = paste(country_list[cnt_n+1],"M",sep = "_")
    others = data.frame(wiot[,c(50:56) + cnt_n*56+5])
    names(others) = c(paste(country_list[cnt_n+1],"N",sep = "_"),
                      paste(country_list[cnt_n+1],"O",sep = "_"),
                      paste(country_list[cnt_n+1],"P",sep = "_"),
                      paste(country_list[cnt_n+1],"Q",sep = "_"),
                      paste(country_list[cnt_n+1],"RS",sep = "_"),
                      paste(country_list[cnt_n+1],"T",sep = "_"),
                      paste(country_list[cnt_n+1],"U",sep = "_"))
    
    cnt_final = cbind(A,B,C,D,E,F,G,H,I,J,K,L,M,others)
    if (length(final) == 0) {
      final = data.frame(wiot[,1:5])
    }
    final = cbind(final, cnt_final)
    
  }
  x = data.frame(rowSums(wiot[,c(2470:2689)]))
  names(x) = "Final_Demand"
  
  final = cbind(final , x, wiot[,2690])
  names(final)[ncol(final)] = "TOT"
  
  return(final)
}

select_manu = function(wiot){
  for (i in 1:nrow(wiot)) {
    if (wiot[i,"IndustryCode"] != "C") {
      wiot[i,"Final_Demand"] = 0
    }
  }
  return(wiot)
}

manufac_table = data.frame()

for (year in 2000:2014) {
  load(paste("WIOT",year,"_October16_ROW.RData",sep = ""))
  print(year)
  wiot = fix_rows(wiot)
  wiot = fix_cols(wiot)
  rownames(wiot) = 1:nrow(wiot)
  wiot = select_manu(wiot)
  
  all_zero = which(rowSums(wiot[, -(1:5)]) == 0, arr.ind = TRUE)
  wiot = wiot[-all_zero,-(all_zero+5)]
  rownames(wiot) = 1:nrow(wiot)
  
  
  A_matrix_num = wiot[1:824,6:829]
  A_matrix_dem = wiot[829,6:829]
  
  A_matrix = data.frame()
  
  for (i in 1:nrow(A_matrix_num)) {
    A_matrix = rbind(A_matrix, A_matrix_num[,i]/A_matrix_dem)
  }
  identity_matrix = diag(824)
  A_temp = identity_matrix - as.matrix(A_matrix)
  leontief_inverse = solve(A_temp)
  final_demand = as.matrix(wiot[1:824,"Final_Demand"])
  
  value_added = wiot[827,6:829] / wiot[829,6:829]
  value_added_diag = diag(value_added)
  
  value_production_vector = value_added_diag %*% leontief_inverse %*% final_demand
  
  rownames(value_production_vector) = names(wiot[6:829])
  manufac = value_production_vector[grep("_C", row.names(value_production_vector)),]
  manufac_table = rbind(manufac_table, manufac)
  names(manufac_table) = names(manufac)
  rownames(manufac_table)[nrow(manufac_table)] = year
}

country_iso = c("AUS","AUT","BEL","BGR","BRA","CAN","CHE","CHN","CYP","CZE","DEU","DNK","ESP","EST",
                "FIN","FRA","GBR","GRC","HRV","HUN","IDN","IND","IRL","ITA","JPN","KOR","LTU","LUX",
                "LVA","MEX","MLT","NLD","NOR","POL","PRT","ROU","RUS","SVK","SVN","SWE","TUR","TWN",
                "USA","ROW")
eu_iso = c("AUT","BEL","BGR","CYP","CZE","DEU","DNK","ESP","EST","FIN","FRA","GBR","GRC","HRV",
           "HUN","IRL","ITA","LTU","LUX","LVA","MLT","NLD","POL","PRT","ROU","SVK","SVN","SWE")
other_iso = c("AUS","BRA","CAN","CHE","CHN","IDN","IND","JPN","KOR","MEX","NOR","RUS","TUR","TWN","USA")

manufac_table = round(manufac_table,1)
names(manufac_table) = country_iso

manufac_table_perc = data.frame()
for (i in 1:nrow(manufac_table)){
  temp = round(manufac_table[i,] / sum(manufac_table[i,]) * 100, 1)
  manufac_table_perc = rbind(manufac_table_perc, temp)
}
names(manufac_table_perc) = country_iso

manufac_table_perc_change = manufac_table_perc[15,-c(44)] - manufac_table_perc[1,-c(44)]

countries = data.frame("ISO" = country_iso,
                       "Country" = c("Australia","Austria","Belgium","Bulgaria","Brazil","Canada","Switzerland",
                                     "China","Cyprus","Czech Republic","Germany","Denmark","Spain","Estonia",
                                     "Finland","France","United Kingdom","Greece","Croatia","Hungary","Indonesia",
                                     "India","Ireland","Italy","Japan","Korea","Lithuania","Luxembourg","Latvia",
                                     "Mexico","Malta","Netherlands","Norway","Poland","Portugal","Romania",
                                     "Russia","Slovak Republic","Slovenia","Sweden","Turkey","Taiwan",
                                     "United States","Rest of the World"))



manufac_table_growth = round(manufac_table[15,]/manufac_table[1,]*100,0)

manufac_table_growth_1 = round((manufac_table[15,other_iso]-(manufac_table[1,other_iso]))/manufac_table[1,other_iso]*100,0)
manufac_table_growth_2 = round((rowSums(manufac_table[15,eu_iso])-rowSums(manufac_table[1,eu_iso]))/rowSums(manufac_table[1,eu_iso])*100,0)
manufac_table_growth_3 = round((manufac_table[15,c(1:43)]-manufac_table[1,c(1:43)])/manufac_table[1,c(1:43)]*100,0)
manufac_table_growth = cbind(manufac_table_growth_1,manufac_table_growth_2)
names(manufac_table_growth)[16] = "EU"


manufac_yearly_growth = data.frame()
for (i in 1:(nrow(manufac_table)-1)) {
  temp = round((manufac_table[i+1,]-manufac_table[i,])/manufac_table[i,]*100,0)
  manufac_yearly_growth = rbind(manufac_yearly_growth, temp)
  
}


manuf_table_paper = cbind(manufac_table_perc[c(1,15),other_iso],rowSums(manufac_table_perc[c(1,15),eu_iso]))
names(manuf_table_paper)[16] = "EU"
manuf_table_paper = as.data.frame(t(manuf_table_paper))
manuf_table_paper$difference = manuf_table_paper$`2014`- manuf_table_paper$`2000` #1
manuf_table_paper = cbind(manuf_table_paper,t(manufac_table_growth))
names(manuf_table_paper) = c("GVC income share in 2000 (%)","GVC income share in 2014 (%)",
                             "GVC income share difference (%)","GVC income growth (%)")



#figure 1

plot(manufac_table_perc$TUR,
     main="Figure 1: Manufacturing GVC Income Share (%)",
     ylim=c(0.5,2.5),
     xaxt = "n",
     xlab = "Years",
     ylab = "Manufacturing GVC Income Share (%)",
     type="l")
axis(1, at=1:15, labels=c(2000:2014))
lines(rowMeans(manufac_table_perc[,c(5,21,22,26,30,37,42)]), lty=2)
legend("bottomleft",
       c("Turkey","Developing Countries (Ex. China)"),
       lty=c(1,2))

#figure 2
plot(manufac_yearly_growth$TUR,
     main="Figure 2: Yearly Increase of Manufacturing GVC Income (%)",
     type="l",
     xaxt = "n",
     xlab = "Years",
     ylab = "Yearly Increase of Manufacturing GVC Income (%)")
axis(1, at=1:14, labels=c(2001:2014))
lines(rowMeans(manufac_yearly_growth[,c(5,21,22,26,30,37,42)]), lty=2)
lines(rowMeans(manufac_yearly_growth[,-c(44,41)]), lty=2, lwd=3)
legend("bottomleft",
       c("Turkey","Developing Countries (Ex. China)","World Average"),
       lty=c(1,2,2),
       lwd=c(0,0,3))


