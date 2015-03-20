package peg_game_scala

class BoardState(size:Int) {
  var board = Array.fill[Boolean](size,size) { false }
  for (i <- 0 until board.length)
    for (j <- 0 to i)
      board(i)(j) = true
  board(0)(0) = false
  override def toString(): String = {
    var str = ""
    for (i <- 0 until board.size) {
      str += " " * (board.size - i)
      for (j <- 0 to i)
        if (board(i)(j)) str += " *" else str += " -"
      str += "\n"
    }
    str
  }

  def isSolved(): Boolean = {
    var numPegs = 0
    for (i <- 0 until board.length) {
      for (j <- 0 to i) {
        if (board(i)(j)) {
          numPegs += 1
          if (numPegs >= 2) {
            return false
          }
        }
      }
    }
    true
  }
  
  def getSucessors(): scala.collection.mutable.Stack[BoardState] = {
    val success = new scala.collection.mutable.Stack[BoardState]
    def getNeighbors(R:Int, C:Int) =
          for {
            neighborRoff <- -1 to +1
            neighborCoff <- -1 to +1
            if ( (neighborRoff + neighborCoff) != 0)
          } yield (neighborRoff + R, neighborCoff + C)
    def goodPosition(pos: (Int,Int)) = pos._1 >= 0 && pos._1 < board.size && pos._2 >= 0 && pos._2 <= pos._1

    for (r <- 0 until board.length) {
      for (c <- 0 to r) {
        if (!board(r)(c)) {
          for (neighbor <- getNeighbors(r,c)
               if (goodPosition(neighbor) && board(neighbor._1)(neighbor._2))
               ) {
            val jumper = (neighbor._1 + (neighbor._1 - r), neighbor._2 + (neighbor._2 - c))
            if (goodPosition(jumper) && board(jumper._1)(jumper._2))
              success.push(jump(jumper, neighbor, (r,c)))
          }
        }
      }
    }
    success
  }
  def jump(jumper:(Int, Int), jumpd:(Int, Int), end:(Int, Int)) = {
    val nxt = board.map(_.clone)
    nxt(jumper._1)(jumper._2) = false
    nxt(jumpd._1)(jumpd._2) = false
    nxt(end._1)(end._2) = true
    val it = new BoardState(board.size)
    it.board = nxt
    it
  }
}

object Solve {
  def main(args: Array[String]):Unit = {
    val stack = new scala.collection.mutable.Stack[scala.collection.mutable.Stack[BoardState]]
    var size = if (args.size != 1) {
      println("Run with number of pegs to a side, i.e. 'sbt \"run 5\"', else we will default to 5")
      5
    } else {
      args(0).toInt
    }
    // push on a dummy so we can pop every iteration and still get our parent
    val tmp = scala.collection.mutable.Stack(new BoardState(size))
    tmp.push(new BoardState(size))
    stack.push(tmp)
    while (!stack.isEmpty) {
      val currentFrame = stack.top
      if (currentFrame.size <= 1) {
        stack.pop
      } else {
        // start with a dummy
        currentFrame.pop
        val currentNode = currentFrame.top
        if (currentNode.isSolved) {
          println("Solved!")
          for (step <- stack)
            println(step.top)
          return ()
        }
          // push on a dummy so we can pop every iteration and still get our parent
          val temp = currentNode.getSucessors
          temp.push(new BoardState(size))
          stack.push(temp)
      }
    }
    println("No solution!")
    return ()
  }
}
