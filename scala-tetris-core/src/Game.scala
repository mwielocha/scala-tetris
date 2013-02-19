import java.util.{TimerTask, Timer}

/**
 * Created with IntelliJ IDEA.
 * User: mwielocha
 * Date: 18.02.2013
 * Time: 23:37
 * To change this template use File | Settings | File Templates.
 */
case class Game(board: Board = Board(20, 40, Board.next(10))) {

    def moveDown = Game(board.move(Vector(0, 1)))

    def moveLeft = Game(board.move(Vector(-1, 0)))

    def moveRight = Game(board.move(Vector(1, 0)))

    def rotateRight = Game(board.rotateRight)

    def rotateLeft = Game(board.rotateLeft)

    def tap = Game(board.tap)

    def tick = Game(board.tick)

}
