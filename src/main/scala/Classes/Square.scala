package Classes

trait Square:

  private var projectiles = Vector[Projectile]()
  //Check whether the square is available for placing Towers, return true id the square is empty
  def isEmpty(): Boolean

  //Keep track of bullets entering and exiting the squre
  def bullets(): Vector[Projectile] = projectiles

  //Add projectiles to square
  def add_Projectile(bullet: Projectile): Boolean =
    projectiles = projectiles.concat(Seq(bullet))
    if projectiles.contains(bullet) then true
    else false

  //Clear the projectile from the square
  def clear(bullet: Projectile): Boolean =
    projectiles = projectiles.filter(_ != bullet)
    if projectiles.contains(bullet) then false
    else true

end Square

class Path extends Square:

  private var enemies: Vector[Enemy] = Vector()

  def isEmpty() = false

  def enemy = enemies

  def add_Enemy(enemy: Enemy): Boolean =
    enemies = enemies.concat(Vector(enemy))
    if enemies.contains(enemy) then true
    else false

  def remove_Enemy(enemy: Enemy): Boolean =
    enemies = enemies.filter(_ != enemy)
    if enemies.contains(enemy) then false
    else true
    

end Path

class Other extends Square:

  private var tower: Option[Tower] = None

  private var obstacle: Option[Obstacle] = None

  def Tower = tower

  def Obstacle = obstacle

  def isEmpty() = if tower.isDefined || obstacle.isDefined then false else true

  def add_Tower(new_tower: Tower): Boolean =
    if this.isEmpty() then
      tower = Option(new_tower)
      true
    else false

  def remove_Tower(new_tower: Tower): Boolean =
    if !this.isEmpty() then
      tower = None
      true
    else false

  def add_Obstacle(new_obstacle: Obstacle): Boolean =
    if this.isEmpty() then
      obstacle = Option(new_obstacle)
      true
    else false

  def remove_Obstacle(new_obstacle: Obstacle): Boolean =
    if !this.isEmpty() then
      obstacle = None
      true
    else false

end Other