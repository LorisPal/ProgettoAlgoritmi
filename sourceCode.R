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
          message("Nodo trovato ", node)
        }
        else {
          message("Nodo non trovato")
        }
      },
      
      addNode = function(x) {
        if (length(nodo) < 1) {
          nodo <<- c(nodo, x)
          message(x, " aggiunto")
        }
        else{
          searchNode(x)
          if (!trovato) {
            nodo <<- c(nodo, x)
            message(x, " aggiunto")
          }
        }
      },
      
      searchArch = function(NodeP, NodeA) {
        trovato <<- FALSE
        for (i in 1:nrow(arco)) {
          if (isTRUE(NodeP == arco[[i, "Partenza"]] &&
                     NodeA == arco[[i, "Arrivo"]])) {
            trovato <<- TRUE
            break
          }
        }
        if (trovato) {
          message("Arco trovato")
        }
        else {
          message("Arco non trovato")
        }
      },
      
      addArco = function(x, nodoP, nodoA) {
        searchArch(nodoP, nodoA)
        if (length(arco) < 1) {
          newRow <-
            data.frame(peso = x,
                       Partenza = nodoP,
                       Arrivo = nodoA)
          arco <<- rbind(arco, newRow)
          message(x, " ", nodoP, " ", nodoA, " aggiunto")
          
        }
        else{
          searchArch(nodoP, nodoA)
          if (!trovato) {
            newRow <-
              data.frame(peso = x,
                         Partenza = nodoP,
                         Arrivo = nodoA)
            arco <<- rbind(arco, newRow)
            message(x, " ", nodoP, " ", nodoA, " aggiunto")
          }
        }
      },
      
      buildMatrix = function() {
        matrice <<- matrix(NULL, length(nodo), length(nodo))
        rownames(matrice) <<- c(nodo)
        colnames(matrice) <<- c(nodo)
        for (j in 1:length(nodo)) {
          for (i in 1:length(nodo)) {
            searchArch(nodo[[i]], nodo[[j]])
            if(trovato){
              matrice[[i,j]] <<- arco[[i, "peso"]]
            }
          }
        }
        print(matrice)
      }
    )
  )