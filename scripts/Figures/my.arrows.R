my.arrows <- function(arrow.coordinates, arr.width = 1, arr.point.length, arr.fill = "grey", arr.border = "black", lty = 1){
  
  
  list2env(arrow.coordinates, envir = .GlobalEnv)
  
  
  # 
  # x0 = 5
  # x1 = 4
  # y0 = 5
  # y1 = 4
  # 
  # arr.width = 2
  
  
  down <- y1 < y0
  
  
  
  O <- matrix(c(x0, y0), nrow = 1)
  E <- matrix(c(x1, y1), nrow = 1)
  
  OE.coordinates <- rbind(O, E)
  
  # plot(OE.coordinates,  xlim = c(0, 10), ylim = c(0, 10), pch = c(1, 16))
  
  if(down){
    E <- rotatexy(E, angle = +180, mid = O)
    x1 <- E[,1]
    y1 <- E[,2]
  }
  
  OE.coordinates <- rbind(O, E)
  
  M <- dist(OE.coordinates, method = "euclidean") # OE distance
  
  rotation.arrow.rad <- acos((x1 - x0) / M) # in rad
  rotation.arrow.degrees <-  (rotation.arrow.rad *180)/pi # in degrees
  
  
  O.prim.x <- O[1] + (cos(rotation.arrow.rad) * arr.width/2)
  O.prim.y <- O[2] + (sin(rotation.arrow.rad) * arr.width/2)
  
  O.prim <- matrix(c(O.prim.x, O.prim.y), nrow = 1)
  
  A <- rotatexy(O.prim, angle = +90, mid = O, asp = T)
  B <- rotatexy(O.prim, angle = -90, mid = O, asp = T)
  
  
  E.prim.x <- E[1] - (cos(rotation.arrow.rad ) * M/5 * sqrt(arr.width))
  E.prim.y <- E[2] - (sin(rotation.arrow.rad) * M/5 * sqrt(arr.width))
  
  E.prim <- matrix(c(E.prim.x, E.prim.y), nrow = 1)
  
  
  C.G.prim.x <- E.prim[1] + (cos(rotation.arrow.rad) * arr.width/2)
  C.G.prim.y <- E.prim[2] + (sin(rotation.arrow.rad) * arr.width/2)
  
  C.G.prim <- matrix(c(C.G.prim.x, C.G.prim.y), nrow = 1)
  
  G <- rotatexy(C.G.prim, angle = +90, mid = E.prim, asp = T)
  C <- rotatexy(C.G.prim, angle = -90, mid = E.prim, asp = T)
  
  
  F.D.prim.x <- E.prim[1] + (cos(rotation.arrow.rad) * arr.width/2 * 1.5)
  F.D.prim.y <- E.prim[2] + (sin(rotation.arrow.rad) * arr.width/2 * 1.5)
  
  F.D.prim <- matrix(c(F.D.prim.x, F.D.prim.y), nrow = 1)
  
  F. <- rotatexy(F.D.prim, angle = +90, mid = E.prim, asp = T)
  D <- rotatexy(F.D.prim, angle = -90, mid = E.prim, asp = T)
  
  
  
  
  # # D.x <-  E.pr[1] + (cos(rotation.arrow.rad ) *OE.prim.dist)
  # D <- rotatexy(E, angle = +90, mid = E.prim, asp = T)
  # F. <- rotatexy(E, angle = -90, mid = E.prim, asp = T)
  # 
  # 
  # OE.prim.dist <- dist(rbind(O, E.prim), method = "euclidean")
  # 
  # G.x <-  A[1] + (cos(rotation.arrow.rad ) *OE.prim.dist)
  # G.y <-  A[2] + (sin(rotation.arrow.rad ) *OE.prim.dist)
  # 
  # # G <- matrix(c(G.x, G.y), nrow = 1)
  # 
  # C.x <-  B[1] + (cos(rotation.arrow.rad ) *OE.prim.dist)
  # C.y <-  B[2] + (sin(rotation.arrow.rad ) *OE.prim.dist)
  # 
  # # C <- matrix(c(C.x, C.y), nrow = 1)
  
  all.coordinates <- rbind(A, B, C, D, E, F., G)
  
  if(down){
    all.coordinates <- rotatexy(all.coordinates, angle = 180, mid = O, asp = F)
  }
  
  
  
  # plot(OE.coordinates,  xlim = c(0, 10), ylim = c(0, 10), type = "n")
  polygon(x = all.coordinates[,1], y = all.coordinates[,2], col = arr.fill, border = arr.border, lty = lty)
  
}
