import java.util.Random

/**
 * Created with IntelliJ IDEA.
 * User: mwielocha
 * Date: 18.02.2013
 * Time: 20:37
 * To change this template use File | Settings | File Templates.
 */
case class Board(width: Int, height: Int, block: Seq[Seq[Vector]], construct: Seq[Vector] = Seq()) {

    def update(newBlock: Seq[Seq[Vector]], newConstruct: Seq[Vector] = construct) = Board(width, height, newBlock, newConstruct)

    def move(v: Vector) = {
        val altered = block.map(_.map(_ + v))
        if (check(altered.head)) update(altered) else this
    }

    def rotateRight = {
        val altered: Seq[Seq[Vector]] = block.tail :+ block.head
        if (check(altered.head)) update(altered) else this
    }

    def rotateLeft = {
        val altered: Seq[Seq[Vector]] = block.last +: block.init
        if (check(altered.head)) update(altered) else this
    }

    def check(shape: Seq[Vector]): Boolean = {
        (shape.map(_.x).max < width) && (shape.map(_.x).min > -1) && (shape.map(_.y).max < height) && (disjoins(shape))
    }

    def tick = {
        val ticked = block.map(_.map(_ + Vector(0, 1)))
        if(disjoins(ticked.head) && (ticked.head.map(_.y).max < height)) {
            update(ticked)
        } else {
            update(Board.next(width / 2), construct ++ block.head).checkRemove(block.head)
        }
    }

    def tap = {
        val yaxis: Seq[Int] = block.head.map(_.y)
        val maxy = yaxis.max
        val y = block.head.filter(_.y == maxy).foldLeft(height - maxy) {
            (n: Int, v: Vector) => {
                construct.filter(_.x == v.x).map(_.y) match {
                    case Nil => n
                    case any => math.min(n, any.min - maxy)
                }
            }
        }
        println(y)
        move(Vector(0, y - 1))
    }

    def disjoins(shape: Seq[Vector]): Boolean = construct.intersect(shape).isEmpty

    def checkRemove(shape: Seq[Vector]): Board = {
        shape.map(_.y).distinct.foldLeft(Seq[Int]()) {
            (result: Seq[Int], y: Int) => {

                construct.distinct.filter(_.y == y).size match {
                    case `width` => result :+ y
                    case _ => result
                }
            }
        } match {
            case Nil => this
            case axis => {
                val filtered = construct.filterNot((v: Vector) => axis.contains(v.y))
                val newConstruct = filtered.filter(_.y > axis.max) ++ (filtered.filter(_.y < axis.min).map(_ + Vector(0, axis.size)))
                update(block, newConstruct)
            }
        }
    }
}

object Board {

    val blocks = Seq(
        Seq( // L
            Seq(
                Vector(0, 0),
                Vector(0, 1),
                Vector(1, 1),
                Vector(2, 1)
            ),
            Seq(
                Vector(1, 0),
                Vector(2, 0),
                Vector(1, 1),
                Vector(1, 2)
            ),
            Seq(
                Vector(0, 0),
                Vector(1, 0),
                Vector(2, 0),
                Vector(2, 1)
            ),
            Seq(
                Vector(1, 0),
                Vector(1, 1),
                Vector(1, 2),
                Vector(0, 2)
            )
        ),
        Seq( // I
            Seq(
                Vector(0, 1),
                Vector(1, 1),
                Vector(2, 1),
                Vector(3, 1)
            ),
            Seq(
                Vector(2, 0),
                Vector(2, 1),
                Vector(2, 2),
                Vector(2, 3)
            )
        ),
        Seq( // O
            Seq(
                Vector(0, 0),
                Vector(0, 1),
                Vector(1, 0),
                Vector(1, 1)
            )
        ),
        Seq( // T
            Seq(
                Vector(0, 1),
                Vector(1, 1),
                Vector(2, 1),
                Vector(1, 0)
            ),
            Seq(
                Vector(1, 0),
                Vector(1, 1),
                Vector(2, 1),
                Vector(1, 2)
            ),
            Seq(
                Vector(0, 1),
                Vector(1, 1),
                Vector(2, 1),
                Vector(1, 2)
            ),
            Seq(
                Vector(1, 0),
                Vector(1, 1),
                Vector(0, 1),
                Vector(1, 2)
            )
        )
    )

    def next(x: Int): Seq[Seq[Vector]] = {
        val rand = new Random(System.currentTimeMillis())
        val index = rand.nextInt(blocks.length)
        blocks(index).map(_.map(_ + Vector(x, 0)))
    }

}

case class Vector(x: Int, y: Int) {

    def +(other: Vector) = Vector(x + other.x, y + other.y)
    def -(other: Vector) = Vector(x - other.x, y - other.y)
    def *(other: Vector) = Vector(x * other.x, y * other.y)


}

