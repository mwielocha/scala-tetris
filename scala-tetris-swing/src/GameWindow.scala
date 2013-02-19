/**
 * Created with IntelliJ IDEA.
 * User: mwielocha
 * Date: 18.02.2013
 * Time: 21:08
 * To change this template use File | Settings | File Templates.
 */


import java.util.TimerTask
import swing._
import event.KeyPressed
import java.awt.{BorderLayout, Color}
import scala.swing.event.Key
import java.awt.event._


object GameWindow extends SimpleSwingApplication {

    val rectSize = 20

    var game: Game = Game()

    def top = new MainFrame() {
        title = "Scala Tetris"
        minimumSize = new Dimension(600, 900)
        contents = new BorderPanel {
            override def paintComponent(g: Graphics2D) = {
                super.paintComponent(g)

                def drawGrid(x: Int, y: Int) {
                    g.setColor(new Color(77, 165, 209))
                    (0.to(x, rectSize)).foreach(z => g.drawLine(z, 0, z, y))
                    (0.to(y, rectSize)).foreach(z => g.drawLine(0, z, x, z))
                }

                def drawRectangle(v: Vector) {
                    g.setColor(new Color(44, 106, 138))
                    g.fillRect(
                        (v.x * rectSize) + 1,
                        (v.y * rectSize) + 1,
                        rectSize - 1,
                        rectSize - 1)
                }

                drawGrid(game.board.width * rectSize, game.board.height * rectSize)
                game.board.block.head.foreach(drawRectangle)
                game.board.construct.foreach(drawRectangle)
            }


            listenTo(keys)
            reactions += {

                case KeyPressed(_, Key.Left, _, _) =>
                    game = game.moveLeft
                    repaint()
                case KeyPressed(_, Key.Right, _, _) =>
                    game = game.moveRight
                    repaint()
                case KeyPressed(_, Key.Down, _, _) =>
                    game = game.moveDown
                    repaint()
                case KeyPressed(_, Key.W, _, _) =>
                    game = game.rotateRight
                    repaint()
                case KeyPressed(_, Key.Q, _, _) =>
                    game = game.rotateLeft
                    repaint()
                case KeyPressed(_, Key.Space, _, _) =>
                    game = game.tap
                    repaint()

            }

            focusable = true
            requestFocus()

            import javax.swing.{AbstractAction, Timer}

            val viewTimer = new Timer(100, new AbstractAction() {
                def actionPerformed(e: ActionEvent) { repaint }
            })
            viewTimer.start

        }
    }

    import java.util.Timer

    val gameTimer = new Timer

    gameTimer.schedule(new TimerTask {
        def run() {
            game = game.tick
        }
    }, 0, 500);
}
