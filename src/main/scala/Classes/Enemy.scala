package Classes

import o1.grid.*

abstract class Enemy(id: String, Health: Double, Speed: Double, damage: Int, Trait: Vector[String], money: Int, intial_pos: GridPos, game: Game):
  private var health = Health
  private var speed = Speed
  private var Traits = Trait
  var position = intial_pos
  var distance = 0
  private var on_Fire = false
  private var is_Goo = false
  private var frozen = false
  private var frozen_counter = 0
  private var Goo_counter = 0
  private var Fire_counter = 0

  def Healths = health
  
  def traits = Traits
  
  def Speeds = speed
  
  def Money = money

  def Move(): Boolean =
    if !frozen then
      if on_Fire then
        Fire_counter += 1
        this.health = this.health - 2
        if Fire_counter == 10 then
          on_Fire = false
          Fire_counter = 0
      if is_Goo then
        Goo_counter += 1
        if Goo_counter == 10 then
          is_Goo = false
          speed = speed * 2
          Goo_counter = 0
      game.apply(position) match
        case n: Path =>
          n.remove_Enemy(this)
          distance += 1
          if distance + 1 >= game.path.size then
            game.Remove_Health(damage)
            game.remove_Enemy(this)
            true
          else
            game.apply(game.path(distance)) match
            case m: Path =>
              position = game.path(distance)
              m.add_Enemy(this)
            case m: Other => false
        case n: Other => false
    else
      frozen_counter += 1
      if frozen_counter == 7 then
        frozen = false
        frozen_counter = 0
      false

  def IsDead(): Boolean = health <= 0

  def Get_Pos() = position

  def Get_Hit(damage: Double, projectile: Projectile): Boolean =
    this.health = this.health - damage
    if !IsDead() then
      projectile match
        case n:Fire => 
          if !this.Traits.contains("Fire immunity") then
            on_Fire = true
          true
        case n:Bomb => 
          if !this.Traits.contains("Fire immunity") then
            on_Fire = true
          true
        case n:Goo =>
          is_Goo = true
          speed = speed / 2
          true
        case n:Ice => 
          if !this.Traits.contains("Ice immunity") then
            frozen = true
          true
        case _ => true
    else  true
      
  def Die(): Boolean =
    if IsDead() then
      game.apply(position) match
        case n: Path =>
          n.remove_Enemy(this)
          game.add_Money(Money)
          true
        case n: Other => false
    else false

end Enemy

case class Basic(id: String, t: Vector[String],pos: GridPos, game: Game) extends Enemy(id,10,10,1,t,5,pos,game)

case class Fast(id: String, t: Vector[String], pos: GridPos, game: Game) extends Enemy(id,15,7,2,t,10,pos,game)

case class Big(id: String, t: Vector[String], pos: GridPos, game: Game) extends Enemy(id,30,12,5,t,10,pos,game)

case class Faster(id: String, t: Vector[String], pos: GridPos, game: Game) extends Enemy(id,25,5,5,t,20,pos,game)

case class Bigger(id: String, t: Vector[String], pos: GridPos, game: Game) extends Enemy(id,50,15,10,t,20,pos,game)

case class Carrier(id: String, t: Vector[String], pos: GridPos, carry: Vector[Enemy], game: Game) extends Enemy(id,60,20,20,t,50,pos,game):
  
  override def Die(): Boolean =
    if IsDead() then
      game.apply(Get_Pos()) match
        case n: Path =>
          n.remove_Enemy(this)
          for i <- carry do
            i.position = Get_Pos()
            i.distance = this.distance
            n.add_Enemy(i)
            game.add_Enemy(i)
          game.add_Money(Money)
          true
        case n: Other => false
    else false

end Carrier
