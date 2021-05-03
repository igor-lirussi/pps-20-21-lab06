package u06lab.code

object TicTacToe extends App{
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark] //alias
  type Game = List[Board] //alias

  def find(board: Board, x: Int, y: Int): Option[Player] = board.find(m => m.x == x && m.y == y).map(m=> m.player)
  //vecchia implementazione, ricordarsi che la find passa già un Option, usare la map per entrare dentro gli eventuali risultati, non la get
  //def find(board: Board, x: Int, y: Int): Option[Player] = if (board.exists(mk => mk.x==x && mk.y==y))  Some(board.find(mk => mk.x==x && mk.y==y).get.player) else None

  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for(x <- 0 to 2;
        y <- 0 to 2
        if find(board,x,y).isEmpty)
      yield board:+Mark(x,y,player)
  }
  /*
  //ricordarsi di usare il for per trovare tutti i posti, filtrando con l'if, in automatico ritorna una sequenza di tutte le mosse concatenate alla board
  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    var seq: Seq[Board] = Seq[Board]()
    //per tutti i posti della board, aggiungi alla lista delle board possibili la board passata con la mossa del player sul posto disponibile
    getPlacesAvailable(board).foreach(availPlaceTup => seq=seq.+:( board.+:( Mark(availPlaceTup._1,availPlaceTup._2,player)) ) )
    seq
  }
  */


  def computeAnyGame(player: Player, moves: Int): Stream[Game] = moves match {
  case 0 => Stream(List(Nil)) //1 game come lista di 0 board
  case _ => for {
    game <- computeAnyGame(player.other, moves-1); //tutte i game possibili con X mosse //acquista di volta in volta il valore di un game
    lastBoard = game.head //prendo l'ultima board dal game
    //il prossimo step è ritornare per ogni board finale
    //tutte le board possibili con una mossa
    nextMove <- placeAnyMark(lastBoard, player) //acquista di volta in volta il valore di una mossa fattibile, una board valida
    } yield { //for ritorna una lista di game uno a uno e di prossime mosse una a una

      if( winning(game.head).isDefined ) //se il game è vincente lo ritorno così
        game
      else
        nextMove :: game //combinazione cartesiana di tutte le mosse e tutti i game
    }

}


  def printBoards(game: Seq[Board]): Unit =
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }


  def isPlaceAvailable(board: Board, x: Int, y: Int): Boolean = !board.exists(mark => mark.x==x && mark.y==y) //non esiste il mark alla posizione

  def getPlacesAvailable(board:Board): List[(Int, Int)] = {
    var placesAvailable :List[(Int, Int)] = List[(Int,Int)]()
    for (x<- 0 to 2;y<- 0 to 2) { if(isPlaceAvailable(board,x,y)) placesAvailable=placesAvailable.+:(x,y) }
    placesAvailable
  }

  def winning(board: Board): Option[Player] = {
    for(x <- 0 to 2;
        y <- 0 to 2;
        player <- find(board,x,y) ) { //fornisce tutte le combinazioni una alla volta
        //per ognuna
        if ( find(board,x,y) == find(board,x+1.abs,y) && find(board,x,y) == find(board,x+2.abs,y) //player == stessa colonna   oppure
        ||   find(board,x,y) == find(board,x,y+1.abs) && find(board,x,y) == find(board,x,y+2.abs) //player == stessa riga
          ) {
          return Some(player)
        }
    }
    if (find(board,0,0) == find(board,1,1) && find(board,1,1) == find(board,2,3)) return find(board,1,1)  //diagonale |\|
    if (find(board,2,0) == find(board,1,1) && find(board,1,1) == find(board,0,2)) return find(board,1,1)  //diagonale |/|
    return None
  }

    // Exercise 1: implement find such that..
      println(find(List(Mark(0,0,X)) ,0,0)) // Some(X)
      println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
      println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

      //test place available
      val exampleBoard1:Board = List(Mark(0,0,X),Mark(0,1,X),Mark(2,2,X))
      printBoards( Seq(exampleBoard1)  ) //stampa una sola board che è la lista dei mark
      println(isPlaceAvailable(exampleBoard1,0,0))//false
      println(isPlaceAvailable(exampleBoard1,1,1))//true
      println(isPlaceAvailable(List(Mark(0,0,O)),0,0))//false

      //test get PlacesAvailable
      println(getPlacesAvailable(List(Mark(0,0,O), Mark(2,1,X)))) //List((2,2), (2,0), (1,2), (1,1), (1,0), (0,2), (0,1))

      // Exercise 2: implement placeAnyMark such that..
      printBoards(placeAnyMark(List(),X))
      //... ... ..X ... ... .X. ... ... X..
      //... ..X ... ... .X. ... ... X.. ...
      //..X ... ... .X. ... ... X.. ... ...
      printBoards(placeAnyMark(List(Mark(0,0,O)),X))
      //O.. O.. O.X O.. O.. OX. O.. O..
      //... ..X ... ... .X. ... ... X..
      //..X ... ... .X. ... ... X.. ...

      // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
      println("ES 3:")
      computeAnyGame(O, 6) foreach {g => printBoards(g); println()}
      //... X.. X.. X.. XO.
      //... ... O.. O.. O..
      //... ... ... X.. X..
      //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
      //
      //... ... .O. XO. XOO
      //... ... ... ... ...
      //... .X. .X. .X. .X.

      // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
      //test winning
      println("Test WINNING")//None
      println(winning(exampleBoard1))//None
      val exampleBoard2:Board = List(Mark(0,0,X),Mark(0,1,X),Mark(0,2,X)) //X verticale
      println(winning(exampleBoard2))//Some X
      val exampleBoard3:Board = List(Mark(0,1,O),Mark(1,1,O),Mark(2,1,O)) //O orrizzontale
      println(winning(exampleBoard3))//Some O
      val exampleBoard4:Board = List(Mark(1,1,X),Mark(2,0,X),Mark(0,2,X)) //O diagonale /
      println(winning(exampleBoard4))//Some X

}
