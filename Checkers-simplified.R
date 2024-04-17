# Álvaro Amador Herrera Herrera 
rm(list = ls())

jugar_damas <- function(){
  
  #creamos la matriz
  
  matriz_piezas <- matrix(sample(1, 64, replace = TRUE), 8, 8)
  
  # Asigno nombres a las filas y columnas
  rownames(matriz_piezas) <- c("1", "2", "3", "4", "5", "6", "7", "8")
  colnames(matriz_piezas) <- c("1", "2", "3", "4", "5", "6", "7", "8")
  
  # intercalamos 0
  
  for (i in 1:ncol(matriz_piezas)) {
    for (j in 1:nrow(matriz_piezas)) {
      if ((i + j) %% 2 == 0) {
        matriz_piezas[j, i] <- 0
      }
    }
  }
  
  # introducimos las posiciones de las fichas
  
  matriz_piezas[c(1, 3), c(2, 4, 6, 8)] <- 2 
  matriz_piezas[c(2), c(1, 3, 5, 7)] <- 2
  matriz_piezas[c(7), c(2, 4, 6, 8)] <- 3
  matriz_piezas[c(6,8), c(1, 3, 5, 7)] <- 3  
  
  
  
  # función para mover las fichas 
  
  mover_ficha <- function(tablero, fila_origen, columna_origen, fila_destino, columna_destino) {
    # Algunas de las funcionalidades de mover_ficha se aprecían mejor cuando los 
    # jugadores son humanos ya que con los bots los movimientos están controlados
    # Verificamos si las coordenadas están dentro del tablero
    if (fila_origen < 1 || fila_origen > 8 || fila_destino < 1 || fila_destino > 8 || 
        columna_origen < 1 || columna_origen > 8 || columna_destino < 1 || columna_destino > 8) {
      print("Movimiento no válido: coordenadas fuera del tablero.")
      return(tablero)
    }
    
    # Verificamos si la casilla de destino está ocupada
    if (tablero[fila_destino, columna_destino] != 1) {
      print("Movimiento no válido: casilla de destino ocupada.")
      return(tablero)
    }
    
    # Determinamos el color de la ficha que se está moviendo
    ficha <- tablero[fila_origen, columna_origen]
    
    # Comprobamos si la ficha alcanza el borde del tablero contrario
    if (ficha == 2 && fila_destino == 8) {
      # La ficha se convierte en "dama"
      tablero[fila_origen, columna_origen] <- 4
    }
    if (ficha == 3 && fila_destino == 1) {
      # La ficha se convierte en "dama"
      tablero[fila_origen, columna_origen] <- 5
    }

    # Dirección del movimiento (hacia arriba o hacia abajo)
    if (ficha == 2 || ficha == 5) {  # Modificado para incluir ficha 4 Y 5
      if (fila_origen < fila_destino) {  # Las fichas rojas (y damas) se mueven hacia abajo 
        direccion <- 1
      } else {
        print("Movimiento no válido: ficha no reconocida.")
        return(tablero)
      }
    }
    
    if (ficha == 3 || ficha == 4) {  # Modificado para incluir ficha 4 Y 5
      if (fila_origen > fila_destino) {  # Las fichas verdes (y damas) se mueven hacia arriba 
        direccion <- -1
      } else {
        print("Movimiento no válido: ficha no reconocida.")
        return(tablero)
      }
    }
    
    # Movimiento a una casilla válida (diagonal y adyacente)
    if (abs(fila_destino - fila_origen) == 1 && abs(columna_destino - columna_origen) == 1) {
      # Movimiento a una casilla adyacente en diagonal
      # Mover la ficha
      tablero[fila_destino, columna_destino] <- tablero[fila_origen, columna_origen]
      tablero[fila_origen, columna_origen] <- 1
      return(tablero)
    } else if (abs(fila_destino - fila_origen) == 2 && abs(columna_destino - columna_origen) == 2) {
      # Movimiento a una casilla diagonal saltando sobre una ficha rival
      # Verificar si hay una ficha rival en el medio
      fila_intermedia <- (fila_origen + fila_destino) / 2
      columna_intermedia <- (columna_origen + columna_destino) / 2
      if (tablero[fila_intermedia, columna_intermedia] != 1 && tablero[fila_intermedia, columna_intermedia] != ficha) {
        # Mover la ficha
        tablero[fila_destino, columna_destino] <- tablero[fila_origen, columna_origen]
        tablero[fila_origen, columna_origen] <- 1
        # Eliminar la ficha rival
        tablero[fila_intermedia, columna_intermedia] <- 1
        return(tablero)
      } else {
        print("Movimiento no válido: no hay una ficha rival para saltar.")
        return(tablero)
      }
    } else {
      print("Movimiento no válido: movimiento no permitido.")
      return(tablero)
    }
  }
  
  # decidimos empezar con el jugador 2 (fichas rojas)

  turno_jugador <- 2 
  
  # Bucle principal del juego
  
  while (TRUE) {
    # Imprimir el tablero actualizado
    for (i in 1:(nrow(matriz_piezas) + 1)) {
      for (j in 1:(ncol(matriz_piezas) + 1)) {
        if (i == 1 && j == 1) {
          cat("    ") 
        } else if (i == 1) {
          cat(j - 1, "  ") 
        } else if (j == 1) {
          cat(i - 1, "  ") 
        } else {
          if (matriz_piezas[i - 1, j - 1] == 2) {
            cat("\033[30;101m", matriz_piezas[i - 1, j - 1], "\033[0m  ") 
          } else if (matriz_piezas[i - 1, j - 1] == 3) {
            cat("\033[30;104m", matriz_piezas[i - 1, j - 1], "\033[0m  ")  
          } else if (matriz_piezas[i - 1, j - 1] == 1) {
            cat("\033[30m", matriz_piezas[i - 1, j - 1], "\033[0m  ")  
          } else if (matriz_piezas[i - 1, j - 1] == 4) {  # Fichas con valor 4 (dama roja)
            cat("\033[30;101m", matriz_piezas[i - 1, j - 1], "\033[0m  ") 
          } else if (matriz_piezas[i - 1, j - 1] == 5) {  # Fichas con valor 5 (dama verde)
            cat("\033[30;104m", matriz_piezas[i - 1, j - 1], "\033[0m  ")  
          } else {
            cat(matriz_piezas[i - 1, j - 1], " ")
          }
        }
      }
      cat("\n")
    }
    
    # Verificar si algún jugador ha ganado (no hay más fichas del otro jugador)
    if (!any(matriz_piezas %in% c(2, 4))) {
      print("¡Jugador 2 (fichas verdes) ha ganado!")
      break  # Terminar el juego
    } else if (!any(matriz_piezas %in% c(3, 5))) {
      print("¡Jugador 2 (fichas rojas) ha ganado!")
      break  # Terminar el juego
    }
    
    # Determinar el jugador actual y su color de ficha
    if (turno_jugador == 2) {
      jugador_actual <- "rojas"
      color_ficha <- 2
    } else {
      jugador_actual <- "verdes"
      color_ficha <- 3
    }
    
    # Pedir al jugador actual que realice un movimiento (en este caso ambos son bots)
    if (turno_jugador == 2){
      cat("TURNO DE LA MÁQUINA 1\n")  # Mensaje de turno del bot 1
      Sys.sleep(1)
      salida <- 0
      no_mas_movimientos <- 0
      #bot número 1
      while(TRUE){
        
        # aleatorizar movimiento cuando puede comer en ambas direcciones (esto nos
        # permite introducir aleatoriedad)
        for (i in length(which(matriz_piezas %in% c(2, 4))):1) {
          posicion_dos_matriz <- which((matriz_piezas == 2 | matriz_piezas == 4), arr.ind = TRUE)
          fila <- posicion_dos_matriz[i, 1]  
          columna <- posicion_dos_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 8 && matriz_piezas[fila, columna] != 4){
              if (fila <= 6 && columna >= 3 && columna <= 6){
                if (matriz_piezas[fila + 1, columna - 1] == 3 && matriz_piezas[fila + 2, columna - 2] == 1 && matriz_piezas[fila + 1, columna + 1] == 3 && matriz_piezas[fila + 2, columna + 2] == 1){
                  numero <- sample(1:10, 1)
                  if (numero <= 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila + 2
                    columna_destino <- columna - 2
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  } else if (numero > 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila + 2
                    columna_destino <- columna + 2
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  }
                }  
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # aleatorizar movimiento cuando puede avanzar a 1 en ambas direcciones 
        # (esto es lo que más a menudo va a suceder)
        for (i in length(which(matriz_piezas %in% c(2, 4))):1) {
          posicion_dos_matriz <- which((matriz_piezas == 2 | matriz_piezas == 4), arr.ind = TRUE)
          fila <- posicion_dos_matriz[i, 1]  
          columna <- posicion_dos_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 8 && matriz_piezas[fila, columna] != 4){
              if (fila <= 7 && columna >= 3 && columna <= 6){
                if (matriz_piezas[fila + 1, columna - 1] == 1 && matriz_piezas[fila + 1, columna + 1] == 1){
                  numero <- sample(1:10, 1)
                  if (numero <= 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila + 1
                    columna_destino <- columna - 1
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  } else if (numero > 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila + 1
                    columna_destino <- columna + 1
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  }
                }  
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        
        # comer ficha más cercana en las posiciones centrales
        for (i in 1:length(which(matriz_piezas %in% c(2, 4)))) {
          posicion_dos_matriz <- which((matriz_piezas == 2 | matriz_piezas == 4), arr.ind = TRUE)
          fila <- posicion_dos_matriz[i, 1]  
          columna <- posicion_dos_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 8 && matriz_piezas[fila, columna] != 4){
              # comer a la izquierda
              if (fila <= 6 && columna >= 3 && (matriz_piezas[fila + 1, columna - 1] == 3 || matriz_piezas[fila + 1, columna - 1] == 5) && matriz_piezas[fila + 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # comer a la derecha
              } else if (fila <= 6 && columna + 1 < 8 && (matriz_piezas[fila + 1, columna + 1] == 3 || matriz_piezas[fila + 1, columna + 1] == 5) && matriz_piezas[fila + 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            } 
          }
        }
        if (salida == 30) {
          break
        }
        # si no se puede comer a la izquierda o derecha movemos pieza al 1 más cercano
        for (i in 1:length(which(matriz_piezas %in% c(2, 4)))) {
          posicion_dos_matriz <- which((matriz_piezas == 2 | matriz_piezas == 4), arr.ind = TRUE)
          fila <- posicion_dos_matriz[i, 1]  
          columna <- posicion_dos_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 8 && matriz_piezas[fila, columna] != 4){
                # a la izquierda
              if (matriz_piezas[fila + 1, columna - 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # a la derecha
              } else if (matriz_piezas[fila + 1, columna + 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            } 
          }
        }
        if (salida == 30) {
          break
        }
        # para cuando la columna sea la 1
        for (i in 1:length(which(matriz_piezas %in% c(2, 4)))) {
          posicion_dos_matriz <- which((matriz_piezas == 2 | matriz_piezas == 4), arr.ind = TRUE)
          fila <- posicion_dos_matriz[i, 1]  
          columna <- posicion_dos_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna == 1 && matriz_piezas[fila, columna] != 4) {
              # comer a la derecha
              if (fila <= 6 && (matriz_piezas[fila + 1, columna + 1] == 3 || matriz_piezas[fila + 1, columna + 1] == 5) && matriz_piezas[fila + 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover 1 derecha
              } else if (matriz_piezas[fila + 1, columna + 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # para cuando la columna sea la 8
        for (i in 1:length(which(matriz_piezas %in% c(2, 4)))) {
          posicion_dos_matriz <- which((matriz_piezas == 2 | matriz_piezas == 4), arr.ind = TRUE)
          fila <- posicion_dos_matriz[i, 1]  
          columna <- posicion_dos_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna == 8 && matriz_piezas[fila, columna] != 4) {
              # comer a la izquierda
              if (fila <= 6 && (matriz_piezas[fila + 1, columna - 1] == 3 || matriz_piezas[fila + 1, columna - 1] == 5) && matriz_piezas[fila + 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover 1 izquierda
              } else if (matriz_piezas[fila + 1, columna - 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }

        # para tratar con las damas rojas (4)
        for (i in 1:length(which(matriz_piezas == 4))) {
          posicion_cuatro_matriz <- which(matriz_piezas == 4, arr.ind = TRUE)
          fila <- posicion_cuatro_matriz[i, 1]  
          columna <- posicion_cuatro_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila >= 3 && columna >= 3 && columna <= 6 && matriz_piezas[fila, columna] == 4)  {        
              # abajo 
              if (fila <= 3 && columna - 1 > 1 && (matriz_piezas[fila - 1, columna - 1] == 3 || matriz_piezas[fila - 1, columna - 1] == 5) && matriz_piezas[fila - 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # comer a la derecha
              } else if (fila <= 3 && columna + 1 < 8 && (matriz_piezas[fila - 1, columna + 1] == 3 || matriz_piezas[fila - 1, columna + 1] == 5) && matriz_piezas[fila - 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # mover arriba
        for (i in 1:length(which(matriz_piezas == 4))) {
          posicion_cuatro_matriz <- which(matriz_piezas == 4, arr.ind = TRUE)
          fila <- posicion_cuatro_matriz[i, 1]  
          columna <- posicion_cuatro_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila != 1 && columna != 8 && columna != 1 && matriz_piezas[fila, columna] == 4)  {
              # a la izquierda
              if (matriz_piezas[fila - 1, columna - 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # a la derecha
              } else if (matriz_piezas[fila - 1, columna + 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        #comer o moverse para cuando es columna 8
        for (i in 1:length(which(matriz_piezas == 4))) {
          posicion_cuatro_matriz <- which(matriz_piezas == 4, arr.ind = TRUE)
          fila <- posicion_cuatro_matriz[i, 1]  
          columna <- posicion_cuatro_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila >= 3 && columna == 8 && matriz_piezas[fila, columna] == 4)  {      
              # izquierda
              if ((matriz_piezas[fila - 1, columna - 1] == 3 || matriz_piezas[fila - 1, columna - 1] == 5) && matriz_piezas[fila - 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover a la izquierda
              } else if (matriz_piezas[fila - 1, columna - 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        #comer o moverse para cuando es columna 1
        for (i in 1:length(which(matriz_piezas == 4))) {
          posicion_cuatro_matriz <- which(matriz_piezas == 4, arr.ind = TRUE)
          fila <- posicion_cuatro_matriz[i, 1]  
          columna <- posicion_cuatro_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila >= 3 && columna == 1 && matriz_piezas[fila, columna] == 4)  {      
              # comer derecha
              if ((matriz_piezas[fila - 1, columna + 1] == 3 || matriz_piezas[fila - 1, columna + 1] == 5) && matriz_piezas[fila - 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover a la derecha
              } else if (matriz_piezas[fila - 1, columna + 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # si llegan a la fila 1 de nuevo
        for (i in 1:length(which(matriz_piezas == 4))) {
          posicion_cuatro_matriz <- which(matriz_piezas == 4, arr.ind = TRUE)
          fila <- posicion_cuatro_matriz[i, 1]  
          columna <- posicion_cuatro_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila == 1 && matriz_piezas[fila, columna] == 4)  { 
              no_mas_movimientos <- 30
              break
            }
          }
        }

        no_mas_movimientos <- 30
        if (no_mas_movimientos == 30) {
          break
        }
      }
      if (no_mas_movimientos == 30) {
        print("Jugador 1 no tiene movimientos disponibles, jugador 2 gana la partida!")
        break
      }
    } else if (turno_jugador == 3){
      cat("TURNO DE LA MÁQUINA 2\n")  # Mensaje de turno del bot 2
      Sys.sleep(1)
      salida <- 0
      no_mas_movimientos <- 0
      # este va a ser el bot
      while(TRUE){
        # aleatorizar movimiento cuando puede comer en ambas direcciones
        for (i in 1:length(which(matriz_piezas %in% c(3, 5)))) {
          posicion_tres_matriz <- which((matriz_piezas == 3 | matriz_piezas == 5), arr.ind = TRUE)
          fila <- posicion_tres_matriz[i, 1]  
          columna <- posicion_tres_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 1 && matriz_piezas[fila, columna] != 5){
              if (fila >= 3 && columna >= 3 && columna <= 6){
                if (matriz_piezas[fila - 1, columna - 1] == 2 && matriz_piezas[fila - 2, columna - 2] == 1 && matriz_piezas[fila - 1, columna + 1] == 2 && matriz_piezas[fila - 2, columna + 2] == 1){
                  numero <- sample(1:10, 1)
                  if (numero <= 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila - 2
                    columna_destino <- columna - 2
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  } else if (numero > 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila - 2
                    columna_destino <- columna + 2
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  }
                }  
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # aleatorizar movimiento cuando puede avanzar a 1 en ambas direcciones
        for (i in 1:length(which(matriz_piezas %in% c(3, 5)))) {
          posicion_tres_matriz <- which((matriz_piezas == 3 | matriz_piezas == 5), arr.ind = TRUE)
          fila <- posicion_tres_matriz[i, 1]  
          columna <- posicion_tres_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 1 && matriz_piezas[fila, columna] != 5){
              if (fila >= 2 && columna >= 2 && columna <= 7){
                if (matriz_piezas[fila - 1, columna - 1] == 1 && matriz_piezas[fila - 1, columna + 1] == 1){
                  numero <- sample(1:10, 1)
                  if (numero <= 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila - 1
                    columna_destino <- columna - 1
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  } else if (numero > 5){
                    fila_origen <- fila
                    columna_origen <- columna
                    fila_destino <- fila - 1
                    columna_destino <- columna + 1
                    movimientos <- list(fila_origen = fila_origen, 
                                        columna_origen = columna_origen, 
                                        fila_destino = fila_destino, 
                                        columna_destino = columna_destino)
                    fila_origen <- as.integer(movimientos[[1]])
                    columna_origen <- as.integer(movimientos[[2]])
                    fila_destino <- as.integer(movimientos[[3]])
                    columna_destino <- as.integer(movimientos[[4]])
                    salida <- 30
                    break
                  }
                }  
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        
        # comer ficha más cercana en las posiciones centrales
        for (i in 1:length(which(matriz_piezas %in% c(3, 5)))) {
          posicion_tres_matriz <- which((matriz_piezas == 3 | matriz_piezas == 5), arr.ind = TRUE)
          fila <- posicion_tres_matriz[i, 1]  
          columna <- posicion_tres_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 1 && matriz_piezas[fila, columna] != 5){
              # comer a la izquierda
              if (fila >= 3 && columna >= 3 && (matriz_piezas[fila - 1, columna - 1] == 2 || matriz_piezas[fila - 1, columna - 1] == 4) && matriz_piezas[fila - 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # comer a la derecha
              } else if (fila >= 3 && columna <= 6 && (matriz_piezas[fila - 1, columna + 1] == 2 || matriz_piezas[fila - 1, columna + 1] == 4) && matriz_piezas[fila - 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            } 
          }
        }
        if (salida == 30) {
          break
        }
        # si no se puede comer a la izquierda o derecha movemos pieza al 1 más cercano
        for (i in 1:length(which(matriz_piezas %in% c(3, 5)))) {
          posicion_tres_matriz <- which((matriz_piezas == 3 | matriz_piezas == 5), arr.ind = TRUE)
          fila <- posicion_tres_matriz[i, 1]  
          columna <- posicion_tres_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna != 1 && columna != 8 && fila != 1 && matriz_piezas[fila, columna] != 5){
                # a la izquierda
               if (matriz_piezas[fila - 1, columna - 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # a la derecha
              } else if (matriz_piezas[fila - 1, columna + 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            } 
          }
        }
        if (salida == 30) {
          break
        }
        # para cuando la columna sea la 1
        for (i in 1:length(which(matriz_piezas %in% c(3, 5)))) {
          posicion_tres_matriz <- which((matriz_piezas == 3 | matriz_piezas == 5), arr.ind = TRUE)
          fila <- posicion_tres_matriz[i, 1]  
          columna <- posicion_tres_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna == 1 && matriz_piezas[fila, columna] != 5) {
              # comer a la derecha
              if (fila >= 3 && (matriz_piezas[fila - 1, columna + 1] == 2 || matriz_piezas[fila - 1, columna + 1] == 4) && matriz_piezas[fila - 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover 1 derecha
              } else if (matriz_piezas[fila - 1, columna + 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # para cuando la columna sea la 8
        for (i in 1:length(which(matriz_piezas %in% c(3, 5)))) {
          posicion_tres_matriz <- which((matriz_piezas == 3 | matriz_piezas == 5), arr.ind = TRUE)
          fila <- posicion_tres_matriz[i, 1]  
          columna <- posicion_tres_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (columna == 8 && matriz_piezas[fila, columna] != 5) {
              # comer a la izquierda
              if (fila >= 3 && (matriz_piezas[fila - 1, columna - 1] == 2 || matriz_piezas[fila - 1, columna - 1] == 4) && matriz_piezas[fila - 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover 1 izquierda
              } else if (matriz_piezas[fila - 1, columna - 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila - 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # para tratar con las damas verdes (5)
        for (i in 1:length(which(matriz_piezas == 5))) {
          posicion_cinco_matriz <- which(matriz_piezas == 5, arr.ind = TRUE)
          fila <- posicion_cinco_matriz[i, 1]  
          columna <- posicion_cinco_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila != 8 && columna != 1 && columna != 8 && matriz_piezas[fila, columna] == 5)  {        
              # abajo 
              if (fila <= 6 && columna >= 3 && (matriz_piezas[fila + 1, columna - 1] == 2 || matriz_piezas[fila + 1, columna - 1] == 4) && matriz_piezas[fila + 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # comer a la derecha
              } else if (fila <= 6 && columna <= 6  && (matriz_piezas[fila + 1, columna + 1] == 2 || matriz_piezas[fila + 1, columna + 1] == 4) && matriz_piezas[fila + 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
              if (salida == 30) {
                break
              }
              # mover abajo
        for (i in 1:length(which(matriz_piezas == 5))) {
          posicion_cinco_matriz <- which(matriz_piezas == 5, arr.ind = TRUE)
          fila <- posicion_cinco_matriz[i, 1]  
          columna <- posicion_cinco_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila != 8 && columna != 1 && columna != 8 && matriz_piezas[fila, columna] == 5)  {
              # a la izquierda
              if (matriz_piezas[fila + 1, columna - 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # a la derecha
              } else if (matriz_piezas[fila + 1, columna + 1] == 1){
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        #comer o moverse para cuando es columna 1
        for (i in 1:length(which(matriz_piezas == 5))) {
          posicion_cinco_matriz <- which(matriz_piezas == 5, arr.ind = TRUE)
          fila <- posicion_cinco_matriz[i, 1]  
          columna <- posicion_cinco_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila <= 6 && columna == 1 && fila != 8 && matriz_piezas[fila, columna] == 5)  {        
              # derecha
              if ((matriz_piezas[fila + 1, columna + 1] == 2 || matriz_piezas[fila + 1, columna + 1] == 4) && matriz_piezas[fila + 2, columna + 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna + 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover a la derecha
              } else if (matriz_piezas[fila + 1, columna + 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna + 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        #comer o moverse para cuando es columna 8
        for (i in 1:length(which(matriz_piezas == 5))) {
          posicion_cinco_matriz <- which(matriz_piezas == 5, arr.ind = TRUE)
          fila <- posicion_cinco_matriz[i, 1]  
          columna <- posicion_cinco_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila <= 6  && columna == 8 && fila != 8 && matriz_piezas[fila, columna] == 5)  {        
              # izquierda
              if ((matriz_piezas[fila + 1, columna - 1] == 2 || matriz_piezas[fila + 1, columna - 1] == 4) && matriz_piezas[fila + 2, columna - 2] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 2
                columna_destino <- columna - 2
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
                # mover a la izquierda
              } else if (matriz_piezas[fila + 1, columna - 1] == 1) {        
                fila_origen <- fila
                columna_origen <- columna
                fila_destino <- fila + 1
                columna_destino <- columna - 1
                movimientos <- list(fila_origen = fila_origen, 
                                    columna_origen = columna_origen, 
                                    fila_destino = fila_destino, 
                                    columna_destino = columna_destino)
                fila_origen <- as.integer(movimientos[[1]])
                columna_origen <- as.integer(movimientos[[2]])
                fila_destino <- as.integer(movimientos[[3]])
                columna_destino <- as.integer(movimientos[[4]])
                salida <- 30
                break
              }
            }
          }
        }
        if (salida == 30) {
          break
        }
        # si llegan a la fila 8 de nuevo
        for (i in 1:length(which(matriz_piezas == 5))) {
          posicion_cinco_matriz <- which(matriz_piezas == 5, arr.ind = TRUE)
          fila <- posicion_cinco_matriz[i, 1]  
          columna <- posicion_cinco_matriz[i, 2] 
          if (fila >= 1 && fila <= 8 && columna >= 1 && columna <= 8){
            if (fila == 8 && matriz_piezas[fila, columna] == 5)  {
              no_mas_movimientos <- 30
              break
            }
          }
        }
        no_mas_movimientos <- 30
        if (no_mas_movimientos == 30) {
          break
        }
      }
      if (no_mas_movimientos == 30) {
        print("Jugador 2 no tiene movimientos disponibles, jugador 1 gana la partida!")
        break
      }
    }
    # Realizar el movimiento y verificar si es válido
    nueva_matriz <- mover_ficha(matriz_piezas, fila_origen, columna_origen, fila_destino, columna_destino)
    # Si el movimiento es válido, actualizar el tablero y cambiar al turno del
    # siguiente jugador
    if (!identical(nueva_matriz, matriz_piezas)) {
      matriz_piezas <- nueva_matriz
      turno_jugador <- ifelse(turno_jugador == 2, 3, 2)
    }
  }
  
}

jugar_damas()
 




