graficoRc <-
  setRefClass(
    "graficoRc",
    fields = list(
      nodo = "vector",
      arco = "data.frame",
      matrice = "matrix",
      trovato = "logical",
      peso = "numeric"
    ),
    methods = list(
      initialize = function(){
        message("Le funzioni disponibili sono:")
      },
      
      cercaNodo = function(node) {
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
          cercaNodo(x)
          if (!trovato) {
            nodo <<- c(nodo, x)
            message(x, " aggiunto")
          }
        }
      },
      
      cambiaNodo = function(x, v){
        if (length(nodo) < 1) {
          message("Non ci sono nodi da cambiare")
        }
        else {
          cercaNodo(x)
          if (trovato){
            for (i in 1:length(nodo)) {
              if(nodo[[i]]==x){
                nodo[[i]] <<- v
                message("Nodo ", x, " sostituito con ", v)
              }
            }
          }
        }
      },
      
      rimuoviNodo = function(x){
        if (length(nodo) < 1) {
          message("Non ci sono nodi da rimuovere")
        }
        else {
          cercaNodo(x)
          if (trovato){
            nodo <<- nodo[ - which(nodo %in% x)]
            message(x , " rimosso")
          }
        }
      },
      
      cercaArco = function(NodeP, NodeA) {
        trovato <<- FALSE
        for (i in 1:nrow(arco)) {
          if (isTRUE(NodeP == arco[[i, "Partenza"]] &&
                     NodeA == arco[[i, "Arrivo"]])) {
            trovato <<- TRUE
            peso <<- arco[[i, "peso"]]
            break
          }
        }
        if (trovato) {
          message("Arco trovato: ", peso)
        }
        else {
          message("Arco non trovato")
        }
      },
      
      addArco = function(x, nodoP, nodoA) {
        cercaArco(nodoP, nodoA)
        if (length(arco) < 1) {
          newRow <-
            data.frame(peso = x,
                       Partenza = nodoP,
                       Arrivo = nodoA)
          arco <<- rbind(arco, newRow)
          message(x, " ", nodoP, " ", nodoA, " aggiunto")
          
        }
        else{
          cercaArco(nodoP, nodoA)
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
      cambiaArco = function(x, nodoP, nodoA){
        if (length(nodo) < 1) {
          message("Non ci sono archi da cambiare")
        }
        else {
          cercaArco(nodoP, nodoA)
          if (trovato){
            for (i in 1:nrow(arco)) {
              if((arco[[i, "Partenza"]]==nodoP) && (arco[[i, "Arrivo"]]==nodoA)){
                arco[[i, "peso"]] <<- x
                message("Peso arco sostituito con ", x)
              }
            }
          }
        }
      },
      
      elencaVertici = function(){
        vertici <- NULL
        if (length(arco) < 1) {
          message("Non ci sono vertici che hanno archi")
        }
        else{
          for (i in 1:nrow(arco)) {
            if((!is.null(arco[[i, "peso"]])) || (!(arco[[i, "peso"]])==0)){
              vertici[[i]] <- arco[[i, "Arrivo"]]
            }
          }
          message("Lista vertici che hanno un arco:")
          print(vertici)
        }
      },
      
      buildMatrix = function() {
        matrice <<- matrix(0, length(nodo), length(nodo))
        rownames(matrice) <<- c(nodo)
        colnames(matrice) <<- c(nodo)
        for (j in 1:length(nodo)) {
          for (i in 1:length(nodo)) {
            cercaArco(nodo[[i]], nodo[[j]])
            if(trovato){
              matrice[[i,j]] <<- peso
            }
          }
        }
        print(matrice)
      }
    )
  )