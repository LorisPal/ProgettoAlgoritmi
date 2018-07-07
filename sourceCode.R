graficoRc <-
  setRefClass(
    "graficoRc",
    fields = list(
      nodo = "vector",
      arco = "matrix",
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
          print("Nodo trovato: nodo non aggiunto")
        }
        else {
          print("Nodo non trovato: nodo aggiunto")
        }
      },
      
      addNode = function(x) {
        if (length(nodo) < 1) {
          nodo <<- c(nodo, x)
          
        }
        else{
          searchNode(x)
          if (!trovato) {
            nodo <<- c(nodo, x)
          }
        }
      },
      
      addArco = function(x, nodoP, nodoA){
        arco <<- matrix(0, length(nodo), length(nodo))
        rownames(arco) <<- nodo
        colnames(arco) <<- nodo
        for (j in 1:length(nodo)) {
          for (i in 1:length(nodo)) {
            if(nodoP==nodo[[i]] && nodoA==nodo[[j]]){
            arco[[i,j]] <<- x
            }
          }
        }
        print(arco)
      }
    )
  )