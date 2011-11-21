################################
## choix de n couleurs
## on récupère un vecteur des noms de couleurs choisies
## Fonction de Marius Bottin

color.choice = function(n, ordre) {
  plot(0,0,xlim=c(0,26),ylim=c(0,26),pch='',main=paste("Choisir",n,"couleurs en cliquant dessus \n pour les modalités suivantes :\n",ordre))
  xleft=rep(0:25,26)[1:length(colors())]
  ybottom=rep(25:0,each=26)[1:length(colors())]
  xright=xleft+1
  ytop=ybottom+1
  rect(xleft,ybottom,xright,ytop,col=colors())
  tab=rbind(xleft,ybottom,xright,ytop)
  color_ch=character()
  for (i in 1:n) {
    a=locator(1,type='p',pch=substr(as.character(i),1,1))
    if(i>=10){
      text((a$x+.3),a$y,substr(as.character(i),2,2))
    }
    b=which(tab[1,]<a$x & tab[3,]>a$x & tab[2,]<a$y & tab[4,]>a$y)
    color_ch=c(color_ch,colors()[b])
  }
  return(color_ch)
}

##