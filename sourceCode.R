graficoRc <-
  setRefClass(
    "graficoRc",
    fields = list(
      nodo = "vector",
      arco = "data.frame",
      matrice = "matrix",
      trovato = "logical"
    ),
    methods = list(
      searchNode = function(node) {
        trovato <<- FALSE
        for (i in 1:length(nodo)) {
          if (node == nodo[[i]]) {
            trovato <<- TRUE
            break
          }
        }
        if (trovato) {
          message("Nodo trovato: ", node, " non aggiunto")
          message("Matrice di adiacenza: non aggiornata")
        }
        else {
          message("Nodo non trovato: ", node, " aggiunto")
          message("Matrice di adiacenza: aggiornata")
        }
      },
      
      addNode = function(x) {
        if (length(nodo) < 1) {
          nodo <<- c(nodo, x)
          message("Nodo non trovato: ", x, " aggiunto")
          
        }
        else{
          searchNode(x)
          if (!trovato) {
            nodo <<- c(nodo, x)
          }
        }
      },
      
      searchArch = function(y, NodeP, NodeA) {
        trovato <<- FALSE
        for (i in 1:nrow(arco)) {
          if (isTRUE(y == arco[[i, "peso"]] &&
                     NodeP == arco[[i, "Partenza"]] &&
                     NodeA == arco[[i, "Arrivo"]])) {
            trovato <<- TRUE
            break
          }
        }
        if (trovato) {
          message("Arco trovato: ", y, NodeP, NodeA, " non aggiunto")
          message("Matrice di adiacenza: non aggiornata")
        }
        else {
          message("Arco non trovato: ", y, NodeP, NodeA, " aggiunto")
          message("Matrice di adiacenza: aggiornata")
        }
      },
      
      addArco = function(x, nodoP, nodoA) {
        searchArch(x, nodoP, nodoA)
        if (length(arco) < 1) {
          newRow <-
            data.frame(peso = x,
                       Partenza = nodoP,
                       Arrivo = nodoA)
          arco <<- rbind(arco, newRow)
          message("Arco non trovato: ", x, nodoP, nodoA, " aggiunto")
          
        }
        else{
          searchArch(x, nodoP, nodoA)
          if (!trovato) {
            newRow <-
              data.frame(peso = x,
                         Partenza = nodoP,
                         Arrivo = nodoA)
            arco <<- rbind(arco, newRow)
          }
        }
      },
      
      buildMatrix = function() {
        
      }
    )
  )