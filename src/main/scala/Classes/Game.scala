package Classes

import o1.grid.*
import scalafx.beans.property.IntegerProperty
import java.io.*
import java.nio.file.{Files, Paths}
import java.nio.charset.Charset
import scala.io.*
import upickle.default.{macroRW, ReadWriter as RW}

import scala.util.Random

class Game(Map: String, width: Int, height: Int, Starting_Monety: Int, Starting_Health: Int, Round: Int, Difficulty: Int) extends Grid[Square](width,height):
  private var money = Starting_Monety
  val health = IntegerProperty(Starting_Health)
  private var round = Round
  private var paths = Vector[GridPos]()
  private var enemies = Vector[Enemy]()
  private var currentenemies = Vector[Enemy]()
  private var roundEnenmies = Vector[Enemy]()
  private var pointer = 0
  private var towers = Vector[Tower]()
  private var projectiles = Vector[Projectile]()
  private var obstacles = Vector[Obstacle]()
  private var counter = 0
  private var startcheck = false
  
  val rand = new Random()

  def current_Map = Map

  def increaseCounter() = counter += 1

  def roundsStart = startcheck
    
  def remove_Enemy(enemy: Enemy) =
    enemies = enemies.filter(_ != enemy)
    currentenemies = currentenemies.filter(_ != enemy)

  def add_Enemy(enemy: Enemy) =
    currentenemies = currentenemies.concat(Seq(enemy))

  def add_bullets(bullet: Projectile) = projectiles = projectiles.concat(Seq(bullet))
  
  def remove_bullets(bullet: Projectile) = projectiles = projectiles.filter(_ != bullet)
  
  def current_Round = round

  def currentEnemy = currentenemies

  def currentObstacles = obstacles
  
  def Enemy = enemies

  def current_enemy = currentenemies
  
  def bullets = projectiles
  
  def Money = money
  
  def Health = health.value

  def initialElements = for y <- 0 until this.height; x <- 0 until this.width yield Other()

  def create_Board(): Boolean =
    this.initialElements
    if Map == "Map1" then
      paths = paths.concat(Seq(GridPos(5,0),GridPos(5,1),GridPos(5,2),GridPos(5,3),GridPos(6,3),GridPos(7,3),
                               GridPos(8,3), GridPos(9,3),GridPos(10,3),GridPos(11,3),GridPos(12,3),GridPos(13,3),
                               GridPos(14,3),GridPos(15,3), GridPos(15,4),GridPos(15,5),GridPos(15,6),GridPos(15,7),
                               GridPos(14,7),GridPos(13,7),GridPos(12,7),GridPos(11,7),GridPos(10,7),GridPos(9,7),
                               GridPos(8,7),GridPos(7,7),GridPos(6,7),GridPos(5,7),GridPos(5,8),GridPos(5,9),GridPos(5,10),
                               GridPos(5,11),GridPos(6,11),GridPos(7,11), GridPos(8,11), GridPos(9,11),GridPos(10,11),
                               GridPos(11,11),GridPos(12,11),GridPos(13,11), GridPos(14,11),GridPos(15,11),GridPos(15,12),
                               GridPos(15,13),GridPos(15,14),GridPos(15,15),GridPos(14,15),GridPos(13,15),GridPos(12,15),
                               GridPos(11,15),GridPos(10,15),GridPos(9,15),GridPos(8,15),GridPos(7,15),GridPos(6,15),
                               GridPos(5,15),GridPos(5,16),GridPos(5,17),GridPos(5,18),GridPos(5,19)))
      obstacles = obstacles.concat(Seq(Obstacle(rand.nextString(10),GridPos(0,9)),Obstacle(rand.nextString(10),GridPos(1,9)),
                                       Obstacle(rand.nextString(10),GridPos(2,9)),Obstacle(rand.nextString(10),GridPos(3,9)), 
                                       Obstacle(rand.nextString(10),GridPos(4,9)),
                                       Obstacle(rand.nextString(10),GridPos(6,9)),Obstacle(rand.nextString(10),GridPos(7,9)),
                                       Obstacle(rand.nextString(10),GridPos(8,9)),Obstacle(rand.nextString(10),GridPos(9,9)),
                                       Obstacle(rand.nextString(10),GridPos(10,9)),Obstacle(rand.nextString(10),GridPos(11,9)),
                                       Obstacle(rand.nextString(10),GridPos(12,9)),Obstacle(rand.nextString(10),GridPos(13,9)),
                                       Obstacle(rand.nextString(10),GridPos(14,9)),Obstacle(rand.nextString(10),GridPos(15,9)),
                                       Obstacle(rand.nextString(10),GridPos(16,9)),Obstacle(rand.nextString(10),GridPos(17,9)),
                                       Obstacle(rand.nextString(10),GridPos(18,9)),Obstacle(rand.nextString(10),GridPos(19,9))))
    else if Map == "Map2" then
      paths = paths.concat(Seq(GridPos(4,0),GridPos(4,1),GridPos(4,2),GridPos(4,3),GridPos(4,4),GridPos(5,4),
                               GridPos(5,5), GridPos(6,5),GridPos(6,6),GridPos(7,6),GridPos(7,7),GridPos(8,7),
                               GridPos(8,8),GridPos(9,8), GridPos(9,9),GridPos(10,9),GridPos(10,10),GridPos(11,10),
                               GridPos(11,11),GridPos(12,11),GridPos(12,12),GridPos(13,12),GridPos(13,13),GridPos(14,13),
                               GridPos(14,14),GridPos(15,14),GridPos(15,15),GridPos(16,15),GridPos(16,16),GridPos(16,17),GridPos(16,18),
                               GridPos(15,18),GridPos(14,18),GridPos(13,18), GridPos(12,18), GridPos(11,18),GridPos(10,18),
                               GridPos(9,18),GridPos(8,18),GridPos(7,18), GridPos(6,18),GridPos(5,18),GridPos(4,18),
                               GridPos(4,17),GridPos(4,16),GridPos(4,15),GridPos(5,15),GridPos(5,14),GridPos(6,14),
                               GridPos(6,13),GridPos(7,13),GridPos(7,12),GridPos(8,12),GridPos(8,11),GridPos(9,11),
                               GridPos(9,10),GridPos(10,10),GridPos(10,9),GridPos(11,9),GridPos(11,8),GridPos(12,8),GridPos(12,7),
                               GridPos(13,7),GridPos(13,6),GridPos(14,6),GridPos(14,5),GridPos(15,5),GridPos(15,4),GridPos(16,4),
                               GridPos(16,3),GridPos(16,2),GridPos(16,1),GridPos(16,0)))
      obstacles = obstacles.concat(Seq(Obstacle(rand.nextString(10),GridPos(4,5)),Obstacle(rand.nextString(10),GridPos(4,6)),
                                       Obstacle(rand.nextString(10),GridPos(4,7)),Obstacle(rand.nextString(10),GridPos(4,8)),
                                       Obstacle(rand.nextString(10),GridPos(4,9)),Obstacle(rand.nextString(10),GridPos(4,10)),
                                       Obstacle(rand.nextString(10),GridPos(4,11)),Obstacle(rand.nextString(10),GridPos(4,12)),
                                       Obstacle(rand.nextString(10),GridPos(4,13)),Obstacle(rand.nextString(10),GridPos(4,14)),
                                       Obstacle(rand.nextString(10),GridPos(16,5)),Obstacle(rand.nextString(10),GridPos(16,6)),
                                       Obstacle(rand.nextString(10),GridPos(16,7)),Obstacle(rand.nextString(10),GridPos(16,8)),
                                       Obstacle(rand.nextString(10),GridPos(16,9)),Obstacle(rand.nextString(10),GridPos(16,10)),
                                       Obstacle(rand.nextString(10),GridPos(16,11)),Obstacle(rand.nextString(10),GridPos(16,12)),
                                       Obstacle(rand.nextString(10),GridPos(16,13)),Obstacle(rand.nextString(10),GridPos(16,14)),
                                       Obstacle(rand.nextString(10),GridPos(6,4)),Obstacle(rand.nextString(10),GridPos(7,4)),
                                       Obstacle(rand.nextString(10),GridPos(8,4)),Obstacle(rand.nextString(10),GridPos(9,4)),
                                       Obstacle(rand.nextString(10),GridPos(10,4)),Obstacle(rand.nextString(10),GridPos(11,4)),
                                       Obstacle(rand.nextString(10),GridPos(12,4)),Obstacle(rand.nextString(10),GridPos(13,4)),
                                       Obstacle(rand.nextString(10),GridPos(14,4)),Obstacle(rand.nextString(10),GridPos(6,15)),
                                       Obstacle(rand.nextString(10),GridPos(7,15)), Obstacle(rand.nextString(10),GridPos(8,15)),
                                       Obstacle(rand.nextString(10),GridPos(9,15)), Obstacle(rand.nextString(10),GridPos(10,15)),
                                       Obstacle(rand.nextString(10),GridPos(11,15)),Obstacle(rand.nextString(10),GridPos(12,15)),
                                       Obstacle(rand.nextString(10),GridPos(13,15)),Obstacle(rand.nextString(10),GridPos(14,15))))
    else if Map == "Map3" then
      paths = paths.concat(Seq(GridPos(3,0),GridPos(3,1),GridPos(3,2),GridPos(3,3),GridPos(3,4),GridPos(3,5),
                               GridPos(3,6), GridPos(3,7),GridPos(4,7),GridPos(5,7),GridPos(6,7),GridPos(7,7),
                               GridPos(8,7),GridPos(9,7), GridPos(10,7),GridPos(11,7),GridPos(12,7),GridPos(13,7),
                               GridPos(14,7),GridPos(15,7),GridPos(16,7),GridPos(16,6),GridPos(16,5),GridPos(16,4),
                               GridPos(16,3),GridPos(16,2),GridPos(16,1),GridPos(16,0),GridPos(15,0),GridPos(14,0),GridPos(13,0),
                               GridPos(12,0),GridPos(11,0),GridPos(10,0), GridPos(10,1), GridPos(10,2),GridPos(10,3),
                               GridPos(10,4),GridPos(10,5),GridPos(10,6), GridPos(10,7),GridPos(10,8),GridPos(10,9),GridPos(10,10),
                               GridPos(10,11),GridPos(10,12),GridPos(10,13),GridPos(10,14),GridPos(10,15),GridPos(10,16),
                               GridPos(10,17),GridPos(10,18),GridPos(10,19),GridPos(11,19),GridPos(12,19),GridPos(13,19),
                               GridPos(14,19),GridPos(15,19),GridPos(16,19),GridPos(16,18),GridPos(16,17),GridPos(16,16),GridPos(16,15),
                               GridPos(16,14),GridPos(16,13),GridPos(16,12),GridPos(15,12),GridPos(14,12),GridPos(13,12),GridPos(12,12),GridPos(11,12),
                               GridPos(10,12),GridPos(9,12),GridPos(8,12),GridPos(7,12),GridPos(6,12),GridPos(5,12),GridPos(4,12),
                               GridPos(3,12),GridPos(3,13),GridPos(3,14),GridPos(3,15),GridPos(3,16),GridPos(3,17),GridPos(3,18),GridPos(3,19)))
    else if Map == "Map4" then
      paths = paths.concat(Seq(GridPos(1,0),GridPos(1,1),GridPos(1,2),GridPos(1,3),GridPos(1,4),GridPos(1,5),GridPos(1,6),
                               GridPos(1,7),GridPos(1,8),GridPos(1,9),GridPos(1,10),GridPos(1,11),GridPos(1,12),GridPos(1,13),
                               GridPos(1,14),GridPos(1,15),GridPos(1,16),GridPos(1,17),GridPos(1,18),GridPos(2,18),GridPos(3,18),
                               GridPos(4,18),GridPos(5,18),GridPos(6,18),GridPos(7,18),GridPos(8,18),GridPos(9,18),GridPos(10,18),
                               GridPos(11,18),GridPos(12,18),GridPos(13,18),GridPos(14,18),GridPos(15,18),GridPos(16,18),GridPos(17,18),
                               GridPos(17,17),GridPos(17,16),GridPos(17,15),GridPos(17,14),GridPos(17,13),GridPos(17,12),GridPos(17,11),GridPos(17,10),
                               GridPos(17,9),GridPos(17,8),GridPos(17,7),GridPos(17,6),GridPos(17,5),GridPos(17,4),GridPos(17,3),
                               GridPos(17,2),GridPos(16,2),GridPos(15,2),GridPos(14,2),GridPos(13,2),GridPos(12,2),GridPos(11,2),
                               GridPos(10,2),GridPos(9,2),GridPos(8,2),GridPos(7,2),GridPos(6,2),GridPos(6,3),GridPos(6,4),
                               GridPos(6,5),GridPos(6,6),GridPos(6,7),GridPos(6,8),GridPos(6,9),GridPos(6,10),GridPos(6,11),
                               GridPos(6,12),GridPos(6,13),GridPos(6,14),GridPos(6,15),GridPos(7,15),GridPos(8,15),GridPos(9,15),
                               GridPos(10,15),GridPos(11,15),GridPos(11,14),GridPos(11,13),GridPos(11,12),GridPos(11,11),
                               GridPos(11,10),GridPos(11,9),GridPos(12,9),GridPos(13,9),GridPos(14,9),GridPos(15,9),
                               GridPos(16,9),GridPos(17,9),GridPos(18,9),GridPos(19,9)))
    else if Map == "Map5" then
      paths = paths.concat(Seq(GridPos(0,3),GridPos(1,3),GridPos(2,3),GridPos(3,3),GridPos(4,3),GridPos(5,3),GridPos(6,3),
                               GridPos(7,3),GridPos(8,3),GridPos(9,3),GridPos(10,3),GridPos(11,3),GridPos(12,3),GridPos(13,3),
                               GridPos(14,3),GridPos(15,3),GridPos(15,4),GridPos(15,5),GridPos(15,6),GridPos(15,7),GridPos(15,8),
                               GridPos(15,9),GridPos(15,10),GridPos(14,10),GridPos(13,10),GridPos(12,10),GridPos(11,10),
                               GridPos(10,10),GridPos(9,10),GridPos(8,10),GridPos(7,10),GridPos(6,10),GridPos(5,10),
                               GridPos(4,10),GridPos(4,11),GridPos(4,12),GridPos(4,13),GridPos(4,14),GridPos(4,15),GridPos(4,16),
                               GridPos(4,17),GridPos(5,17),GridPos(6,17),GridPos(7,17),GridPos(8,17),GridPos(9,17),GridPos(10,17),
                               GridPos(11,17),GridPos(12,17),GridPos(13,17),GridPos(14,17),GridPos(15,17),GridPos(15,16),
                               GridPos(15,15),GridPos(15,14),GridPos(15,13),GridPos(15,12),GridPos(15,11),GridPos(15,10),
                               GridPos(14,10),GridPos(13,10),GridPos(12,10),GridPos(11,10),GridPos(10,10),GridPos(9,10),
                               GridPos(8,10),GridPos(7,10),GridPos(6,10),GridPos(5,10),GridPos(4,10),GridPos(4,9),
                               GridPos(4,8),GridPos(4,7),GridPos(4,6),GridPos(4,5),GridPos(4,4),GridPos(4,3),GridPos(5,3),
                               GridPos(6,3),GridPos(7,3),GridPos(8,3),GridPos(9,3),GridPos(10,3),GridPos(11,3),GridPos(12,3),
                               GridPos(13,3),GridPos(14,3),GridPos(15,3),GridPos(16,3),GridPos(17,3),GridPos(18,3),GridPos(19,3)))
      obstacles = obstacles.concat(Seq(Obstacle(rand.nextString(10),GridPos(7,6)),Obstacle(rand.nextString(10),GridPos(7,7)),
                                       Obstacle(rand.nextString(10),GridPos(8,6)),Obstacle(rand.nextString(10),GridPos(8,7)),
                                       Obstacle(rand.nextString(10),GridPos(9,6)),Obstacle(rand.nextString(10),GridPos(9,7)),
                                       Obstacle(rand.nextString(10),GridPos(10,6)),Obstacle(rand.nextString(10),GridPos(10,7)),
                                       Obstacle(rand.nextString(10),GridPos(11,6)),Obstacle(rand.nextString(10),GridPos(11,7)),
                                       Obstacle(rand.nextString(10),GridPos(12,6)),Obstacle(rand.nextString(10),GridPos(12,7)),
                                       Obstacle(rand.nextString(10),GridPos(7,13)),Obstacle(rand.nextString(10),GridPos(7,14)),
                                       Obstacle(rand.nextString(10),GridPos(8,13)),Obstacle(rand.nextString(10),GridPos(8,14)),
                                       Obstacle(rand.nextString(10),GridPos(9,13)),Obstacle(rand.nextString(10),GridPos(9,14)),
                                       Obstacle(rand.nextString(10),GridPos(10,13)),Obstacle(rand.nextString(10),GridPos(10,14)),
                                       Obstacle(rand.nextString(10),GridPos(11,13)),Obstacle(rand.nextString(10),GridPos(11,14)),
                                       Obstacle(rand.nextString(10),GridPos(12,13)),Obstacle(rand.nextString(10),GridPos(12,14))))
    else
      false

    for i <- paths do
      this.update(i, Path())
    
    for i <- obstacles do 
      Place_Obstacle(i, i.Pos)

    true
      
  def path = paths
  
  def Place_Enemy(enemy: Enemy, pos: GridPos): Boolean =
    this.apply(pos) match
      case n: Path => n.add_Enemy(enemy)
      case n: Other => false

  def Place_Tower(tower: Tower, pos: GridPos): Boolean =
    this.apply(pos) match
      case n: Other =>
        if money - tower.Cost * Difficulty >= 0 then
          this.remove_Money(tower.Cost * Difficulty)
          towers = towers.concat(Seq(tower))
          n.add_Tower(tower)
        else false
      case n: Path => false
   
  def Sell_Tower(tower: Tower, pos: GridPos): Boolean =
    this.apply(pos) match
      case n: Other => 
        if n.Tower.isDefined then
          this.add_Money(tower.Cost * Difficulty/5)
          towers = towers.filter(_ != tower)
          n.remove_Tower(tower)
          true
        else false
      case n: Path => false

  def Place_Obstacle(obstacle: Obstacle, pos: GridPos): Boolean =
    this.apply(pos) match
      case n: Other => n.add_Obstacle(obstacle)
      case n: Path => false

  def Place_Projectile(bullet: Projectile, pos: GridPos): Boolean = this.apply(pos).add_Projectile(bullet)
  
  def add_Money(amount: Int) = money = money + amount
  
  def remove_Money(amount: Int): Boolean = 
    if money - amount >= 0 then 
      money -= amount 
      true
    else false

  def Remove_Health(amount: Int): Unit = health.value = health.value - amount

  def save_data(gameData: GameData): Unit =
    val updatedData = SaveFormat(List(gameData))

    val writer = Files.newBufferedWriter(Paths.get(s"src/main/resources/saveData/games.json"),Charset.forName("UTF-8"))

    upickle.default.writeTo(updatedData, writer, 2)
    writer.close()
  
  def Save_Game(): Unit =
    val gameData = GameData(round,money,health.value,Map,Difficulty,towers.map(_.toString).toList)
    this.save_data(gameData)
    

  def Start_Round(): Unit =
    if !roundsStart then
      val difficulty = round * 5
      startcheck = true
      var total = 0
      var randomnumber = Vector[Int]()
      while total < difficulty do
        var maxnum = 0
        if round <= 3 then maxnum = 1
        else if round <= 10 then maxnum = 3
        else if round <= 20 then maxnum = 10
        else if round <= 30 then maxnum = 15
        else if round <= 50 then maxnum = 20
        else maxnum = 35
        val num = rand.nextInt(maxnum)
        if total + num + 1 <= difficulty then
          total = total + num + 1
          randomnumber = randomnumber.concat(Seq(num + 1))
        else
          randomnumber = randomnumber.concat(Seq(difficulty - total))
          total = difficulty
      for i <- randomnumber do
        val enemyid = rand.nextString(10)
        i match
          case 1 => enemies = enemies.concat(Seq(Basic(enemyid,Vector(),paths.head,this)))
          case 2 => enemies = enemies.concat(Seq(Fast(enemyid,Vector(),paths.head,this)))
          case 3 => enemies = enemies.concat(Seq(Big(enemyid,Vector(),paths.head,this)))
          case 4 => enemies = enemies.concat(Seq(Basic(enemyid,Vector("Invisible"),paths.head,this)))
          case 5 => enemies = enemies.concat(Seq(Fast(enemyid,Vector("Invisible"),paths.head,this)))
          case 6 => enemies = enemies.concat(Seq(Big(enemyid,Vector("Invisible"),paths.head,this)))
          case 7 => enemies = enemies.concat(Seq(Basic(enemyid,Vector("Fire immunity"),paths.head,this)))
          case 8 => enemies = enemies.concat(Seq(Basic(enemyid,Vector("Ice immunity"),paths.head,this)))
          case 9 => enemies = enemies.concat(Seq(Fast(enemyid,Vector("Fire immunity"),paths.head,this)))
          case 10 => enemies = enemies.concat(Seq(Fast(enemyid,Vector("Ice immunity"),paths.head,this)))
          case 11 => enemies = enemies.concat(Seq(Big(enemyid,Vector("Fire immunity"),paths.head,this)))
          case 12 => enemies = enemies.concat(Seq(Big(enemyid,Vector("Ice immunity"),paths.head,this)))
          case 13 => enemies = enemies.concat(Seq(Faster(enemyid,Vector(),paths.head,this)))
          case 14 => enemies = enemies.concat(Seq(Bigger(enemyid,Vector(),paths.head,this)))
          case 15 => enemies = enemies.concat(Seq(Fast(enemyid,Vector("Invisible", "Fire immunity"),paths.head,this)))
          case 16 => enemies = enemies.concat(Seq(Fast(enemyid,Vector("Invisible", "Ice immunity"),paths.head,this)))
          case 17 => enemies = enemies.concat(Seq(Basic(enemyid,Vector("Invisible", "Fire immunity"),paths.head,this)))
          case 18 => enemies = enemies.concat(Seq(Basic(enemyid,Vector("Invisible", "Ice immunity"),paths.head,this)))
          case 19 => enemies = enemies.concat(Seq(Big(enemyid,Vector("Invisible", "Fire immunity"),paths.head,this)))
          case 20 => enemies = enemies.concat(Seq(Big(enemyid,Vector("Invisible", "Ice immunity"),paths.head,this)))
          case 21 => enemies = enemies.concat(Seq(Faster(enemyid,Vector("Invisible"),paths.head,this)))
          case 22 => enemies = enemies.concat(Seq(Faster(enemyid,Vector("Ice immunity"),paths.head,this)))
          case 23 => enemies = enemies.concat(Seq(Faster(enemyid,Vector("Fire immunity"),paths.head,this)))
          case 24 => enemies = enemies.concat(Seq(Bigger(enemyid,Vector("Invisible"),paths.head,this)))
          case 25 => enemies = enemies.concat(Seq(Bigger(enemyid,Vector("Ice immunity"),paths.head,this)))
          case 26 => enemies = enemies.concat(Seq(Bigger(enemyid,Vector("Fire immunity"),paths.head,this)))
          case 27 => enemies = enemies.concat(Seq(Faster(enemyid,Vector("Invisible", "Ice immunity"),paths.head,this)))
          case 28 => enemies = enemies.concat(Seq(Faster(enemyid,Vector("Invisible", "Fire immunity"),paths.head,this)))
          case 29 => enemies = enemies.concat(Seq(Bigger(enemyid,Vector("Invisible", "Ice immunity"),paths.head,this)))
          case 30 => enemies = enemies.concat(Seq(Bigger(enemyid,Vector("Invisible", "Fire immunity"),paths.head,this)))
          case 31 => enemies = enemies.concat(Seq(Carrier(enemyid,Vector(),paths.head,Vector(Basic(rand.nextString(10),Vector(),paths.head,this),Basic(rand.nextString(10),Vector(),paths.head,this),Basic(rand.nextString(10),Vector(),paths.head,this),Basic(rand.nextString(10),Vector(),paths.head,this),Basic(rand.nextString(10),Vector(),paths.head,this)),this)))
          case 32 => enemies = enemies.concat(Seq(Carrier(enemyid,Vector(),paths.head,Vector(Fast(rand.nextString(10),Vector(),paths.head,this),Fast(rand.nextString(10),Vector(),paths.head,this),Fast(rand.nextString(10),Vector(),paths.head,this),Fast(rand.nextString(10),Vector(),paths.head,this),Fast(rand.nextString(10),Vector(),paths.head,this)),this)))
          case 33 => enemies = enemies.concat(Seq(Carrier(enemyid,Vector(),paths.head,Vector(Big(rand.nextString(10),Vector(),paths.head,this),Big(rand.nextString(10),Vector(),paths.head,this),Big(rand.nextString(10),Vector(),paths.head,this),Big(rand.nextString(10),Vector(),paths.head,this)),this)))
          case 34 => enemies = enemies.concat(Seq(Carrier(enemyid,Vector(),paths.head,Vector(Faster(rand.nextString(10),Vector(),paths.head,this),Faster(rand.nextString(10),Vector(),paths.head,this),Faster(rand.nextString(10),Vector(),paths.head,this)),this)))
          case 35 => enemies = enemies.concat(Seq(Carrier(enemyid,Vector(),paths.head,Vector(Bigger(rand.nextString(10),Vector(),paths.head,this),Bigger(rand.nextString(10),Vector(),paths.head,this),Bigger(rand.nextString(10),Vector(),paths.head,this)),this)))
      roundEnenmies = enemies
    
  def End_Round():Unit =
    money = money + 50
    round = round + 1
    counter = 0
    startcheck = false
    currentenemies = Vector()
    enemies = Vector()
    roundEnenmies = Vector()
    projectiles = Vector()
    pointer = 0

  def tick(): Unit =
    if roundsStart then
      if enemies.nonEmpty || projectiles.nonEmpty || currentenemies.nonEmpty then
        counter = counter + 1
        for i <- currentenemies do
          if i.IsDead() then
            i.Die()
            currentenemies = currentenemies.filter(_ != i)
            enemies = enemies.filter(_ != i)
          else
            if counter % i.Speeds == 1 then i.Move()
        for i <- projectiles do
          if counter % i.Speed == 1 then i.Move()
        for i <- towers do
          if counter % i.Attack_rate == 1 then 
            i.Find_Target()
            i.shoot()
        if counter % 10 == 1 && pointer < roundEnenmies.size then
          currentenemies = currentenemies.concat(Seq(roundEnenmies(pointer)))         
          Place_Enemy(currentEnemy.last, paths.head)
          pointer += 1
      else End_Round()
end Game


case class GameData(
  val Round: Int,
  val Money: Int,
  val Health: Int,
  val Map: String,
  val Difficulty: Int,
  val Towers: List[String]
)

object GameData:
  implicit val rw: RW[GameData] = macroRW

case class SaveFormat(
  val data: List[GameData]
)
object SaveFormat:
  implicit val rw: RW[SaveFormat] = macroRW