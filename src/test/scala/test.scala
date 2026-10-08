import o1.grid
import o1.GridPos
import Classes.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import scala.language.adhocExtensions

class Unit_test extends AnyFlatSpec with Matchers:

  val game = new Game("Map1",50,50,300,100,0,1)
  game.create_Board()
  val corect_path = game.path
  val basic_Tower = Basic_Tower("test",2,3,5,Vector(),GridPos(8,2),game)
  val boomerang_Tower = Boomerang_Tower("test",2,5,5,Vector(),GridPos(7,2),game)
  val enemy = Vector(Basic("test1",Vector(),GridPos(5,0),game),Basic("test2",Vector(),GridPos(5,0),game),Basic("test3",Vector(),GridPos(5,0),game))
  val projectiles = Vector(Basic_Bullet("test1",GridPos(8,2),basic_Tower,GridPos(5,0),game),Boomerang("test2",GridPos(7,2),boomerang_Tower,GridPos(5,0),game))
  "Add_Money" should "return the correct amount" in {
    withClue("Starting money: "){
      game.Money shouldBe 300
    }
    game.add_Money(50)
    withClue("Adding 50 money: "){
      game.Money shouldBe 350
    }
  }

  "create_Board" should "return the correct board" in {
    for i <- 0 until game.width do
      for j <- 0 until game.height do
        if corect_path.contains(GridPos(i,j)) then
          withClue("At position(%d,%d): ".format(i,j)){
            game.apply(GridPos(i,j)) shouldBe a [Path]
          }
        else
          withClue("At position(%d,%d): ".format(i,j)){
            game.apply(GridPos(i,j)) shouldBe a [Other]
          }
    for i <- game.currentObstacles do
      withClue("At position (%d, %d) should have an obstacle".format(i.Pos.x,i.Pos.y)){
        game.apply(i.Pos) shouldBe a [Other]
      }

      withClue("At position (%d, %d) should have an obstacle".format(i.Pos.x,i.Pos.y)){
        game.apply(i.Pos).asInstanceOf[Other].Obstacle.isDefined shouldBe true
      }
  }

  "Place_Tower" should "place the tower correctly" in {
    var initial = game.Money
    game.Place_Tower(basic_Tower,basic_Tower.Get_Pos)

    withClue("At position(8,2): "){
      game.apply(GridPos(8,2)).asInstanceOf[Other].Tower.get shouldBe basic_Tower
    }
    withClue("Money after placing: "){
      game.Money shouldBe initial - 50
    }

    game.Place_Tower(boomerang_Tower,boomerang_Tower.Get_Pos)
    withClue("At position(7,2): "){
      game.apply(GridPos(7,2)).asInstanceOf[Other].Tower.get shouldBe boomerang_Tower
    }
    withClue("Money after placing: "){
      game.Money shouldBe initial - 150
    }
  }

  "Place_Enemy" should "place the enemy correctly" in {
    for i <- enemy do
      game.Place_Enemy(i,GridPos(5,0))
    withClue("At position(5,0): "){
      game.apply(GridPos(5,0)).asInstanceOf[Path].enemy shouldBe enemy
    }
  }

  "Place_Projectile" should "place the projectile correctly" in {
    game.Place_Projectile(projectiles.head,GridPos(8,2))

    withClue("At position(8,2): "){
      game.apply(GridPos(8,2)).bullets() shouldBe Vector(projectiles.head)
    }

    game.Place_Projectile(projectiles(1),GridPos(7,2))
    withClue("At position(7,2): "){
      game.apply(GridPos(7,2)).bullets() shouldBe Vector(projectiles(1))
    }
  }

  "Place_Obstacle" should "place the obstacle correctly" in {
    var newObstacle = Obstacle("test",GridPos(4,3))
    game.Place_Obstacle(newObstacle,GridPos(4,3))
    withClue("At position(4,3): "){
      game.apply(GridPos(4,3)).asInstanceOf[Other].Obstacle.get == newObstacle
    }
  }

  "Enemy_Move" should "function correcty" in {
    enemy.head.Move()

    withClue("After one move, the enemy should move to the next square on its path."){
      enemy.head.Get_Pos() shouldEqual game.path(1)
    }

    enemy.head.Move()
    enemy.head.Move()
    enemy.head.Move()
    enemy.head.Move()

    withClue("After five moves, the enemy should move to the fifth square on its parh"){
      enemy.head.Get_Pos() shouldEqual game.path(5)
    }

    game.remove_Enemy(enemy.head)
  }

  "Tower_Shoot" should "place the projectile correctly" in {
    basic_Tower.Can_See(enemy(1))

    withClue("the target is now defined"){
      basic_Tower.targets.isDefined shouldBe true
    }

    withClue("the target should be the new enemy"){
      basic_Tower.targets.get shouldEqual enemy(1)
    }

    basic_Tower.shoot()
    withClue("the proectile should be on the same tile as the tower"){
      game(basic_Tower.Get_Pos).asInstanceOf[Other].bullets().size shouldEqual 2
    }

    val newEnemy = Basic("test4",Vector("Invisible"),GridPos(8,3),game)
    game.Place_Enemy(newEnemy, GridPos(8,3))
    basic_Tower.Can_See(newEnemy)

    withClue("the tower should not see the invisible enemy"){
      basic_Tower.targets.get shouldEqual enemy(1)
    }

    basic_Tower.add_Trait("invisibility")
    basic_Tower.Can_See(newEnemy)
    withClue("the tower should now see the invisible enemy"){
      basic_Tower.targets.get shouldEqual newEnemy
    }
  }

  "Projectile_Move" should "function correctly" in {

    projectiles.head.Move()
    projectiles(1).Move()

    withClue("basic bullet should move to the next correct position"){
      projectiles.head.position shouldEqual GridPos(6,1)
    }

    withClue("boommerang bullet should move to the next correct position"){
      projectiles(1).position shouldEqual GridPos(6,2)
    }

    projectiles.head.Move()
    withClue("basic bullet should be at the enemy position"){
      projectiles.head.position shouldEqual enemy(1).Get_Pos()
    }

    projectiles.head.Move()
    withClue("the enemy should get hit by the bullet"){
      enemy(1).Healths shouldEqual 8
    }

    projectiles(1).Move()
    projectiles(1).Move()
    projectiles(1).Move()
    withClue("boomerang bullet should be at the enemy position"){
      projectiles(1).position shouldEqual enemy(1).Get_Pos()
    }

    projectiles(1).Move()
    withClue("all enemies on position(5,0) should be hit"){
      enemy(1).Healths shouldEqual 4
      enemy(2).Healths shouldEqual 6
    }
  }