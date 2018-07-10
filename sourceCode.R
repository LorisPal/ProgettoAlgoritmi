graficoRc <-
  setRefClass(
    "graficoRc",
    fields = list(
      nodo = "vector",
      arco = "data.frame",
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
          message("Nodo non trovato: ",node, " aggiunto")
          message("Matrice di adiacenza: aggiornata")
        }
      },
      
      addNode = function(x) {
        if (length(nodo) < 1) {
          nodo <<- c(nodo, x)
          message("Nodo non trovato: ",x, " aggiunto")
          
        }
        else{
          searchNode(x)
          if (!trovato) {
            nodo <<- c(nodo, x)
            arco <<-
              matrix(, length(nodo), length(nodo))
            rownames(arco) <<- (nodo)
            colnames(arco) <<- (nodo)
          }
        }
      },
      
      searchArch = function(y, NodeP, NodeA) {
        trovato <<- FALSE
        for (j in 1:length(nodo)) {
          for (i in 1:length(nodo)) {
            if (isTRUE(NodeP == nodo[[i]] && NodeA == nodo[[j]] && y == arco[[i, j]])) {
              trovato <<- TRUE
            }
          }
        }
        if (trovato) {
          message("Arco trovato: non aggiunto")
          message("Matrice di adiacenza: non aggiornata")
        }
        else {
          message("Arco non trovato: aggiunto")
          message("Matrice di adiacenza: aggiornata")
        }
      },
      
      addArco = function(x, nodoP, nodoA) {
        searchArch(x, nodoP, nodoA)
        for (j in 1:length(nodo)) {
          for (i in 1:length(nodo)) {
            if (nodoP == nodo[[i]] && nodoA == nodo[[j]]) {
              arco[[i, j]] <<- x
            }
          }
        }
        print(arco)
      }
    )
  )