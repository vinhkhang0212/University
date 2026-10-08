package Classes

import o1.grid.*
import scala.util.Random

abstract class Tower(id:String, initial_damage: Int, initial_range: Double, initial_attack_rate: Double, Trait: Vector[String], pos: GridPos, cost: Int, game: Game):

  private var damage = initial_damage

  private var range = initial_range

  private var curret_trait = Trait

  private var attack_rate= initial_attack_rate
  
  private var current_cost = cost

  private var position = pos
  
  private var target: Option[Enemy] = None
  
  private var rand = new Random()
  
  def targets = target
  def Cost = current_cost
  def Damage = damage
  def Range = range
  def Traits = curret_trait
  def Attack_rate = attack_rate
  def upgrades: List[(String, Int)]
  def remove_upgrade(str: String): Unit
  def load_cost(new_cost: Int) = current_cost = new_cost

  def Find_Target(): Unit =
    if game.currentEnemy.nonEmpty then
      if target.isDefined then 
        if target.get.IsDead() then target = None
      for i <- game.currentEnemy do this.Can_See(i)
    else target = None
  
  def Can_See(enemy: Enemy): Unit =
    if !enemy.traits.contains("Invisible") || this.Traits.contains("invisibility") then
      val distance = math.max(math.abs(enemy.Get_Pos().x - this.pos.x),math.abs(enemy.Get_Pos().y - this.pos.y))
      if distance <= range then
        if target.isDefined then
          if game.currentEnemy.contains(target.get) then 
            val current_distance = math.max(math.abs(target.get.Get_Pos().x - this.pos.x),math.abs(target.get.Get_Pos().y - this.pos.y))
            if target.get != enemy && distance < current_distance then target = Some(enemy)
          else target = Some(enemy)
        else
          target = Some(enemy)
      else
        if target.isDefined then
          if target.get == enemy then
            target = None
    else 
      if target.isDefined then
          if target.get == enemy then
            target = None

  def Get_Pos = position

  def add_Trait(new_trait: String): Unit =
    curret_trait = curret_trait.concat(Vector(new_trait))
  
  def add_Cost(amount: Int):Unit = current_cost += amount
  def Upgrade_damage(amount: Int): Unit = damage += amount
  def Upgrade_range(amount: Int): Unit = range += amount
  def Upgrade_attacck_rate(amount: Int): Unit = attack_rate -= amount
  
  def shoot(): Unit

end Tower

case class Basic_Tower(id: String, damage: Int, range: Double, attack_rate: Double, traits: Vector[String], pos: GridPos, game: Game) extends Tower(id,damage,range,attack_rate,traits,pos,50,game):
  private var rand = new Random()
  private var upgrade = List("invisibility" -> 100, "Range" -> 100, "attackrate" -> 100, "Damage" -> 150, "Fire" -> 200, "Range2" -> 200, "attackrate2" -> 200, "Damage2" -> 300)
  def upgrades = upgrade
  def remove_upgrade(str: String) = upgrade = upgrade.filter(_._1 != str)
  
  def shoot(): Unit =
    if targets.isDefined then
      val bulletid = rand.nextString(10)
      if Traits.contains("Fire") then
       val bullet = Fire(bulletid,Get_Pos,this,targets.get.Get_Pos(),game)
       game.add_bullets(bullet)
       game.Place_Projectile(bullet,Get_Pos)
      else
        val bullet = Basic_Bullet(bulletid,Get_Pos,this,targets.get.Get_Pos(),game)
        game.add_bullets(bullet)
        game.Place_Projectile(bullet,Get_Pos)

  override def toString: String =
    var str = "Basic_Tower," + pos + "," + this.Damage + "," + this.Range + "," + this.Attack_rate + "," + this.Cost
    for i <- Traits do str = str + "," +  i
    str
end Basic_Tower

case class Bomb_Tower(id: String, damage: Int, range: Double, attack_rate: Double, traits: Vector[String], pos: GridPos, game: Game) extends Tower(id,damage,range,attack_rate,traits,pos,200,game):
  private var rand = new Random()
  private var upgrade = List("invisibility" -> 100, "Range" -> 100, "attackrate" -> 100, "Damage" -> 150, "Range2" -> 200, "Damage2" -> 300, "attackrate2" -> 200)
  def remove_upgrade(str: String) = upgrade = upgrade.filter(_._1 != str)
  def upgrades = upgrade
  def shoot(): Unit =
    if targets.isDefined then
      val bulletid = rand.nextString(10)
      val bullet = Bomb(bulletid,Get_Pos,this,targets.get.Get_Pos(),game)
      game.add_bullets(bullet)
      game.apply(Get_Pos).add_Projectile(bullet)
  override def toString: String =
    var str = "Bomb_Tower," + pos + "," + this.Damage + "," + this.Range + "," + this.Attack_rate + "," + this.Cost
    for i <- Traits do str = str + "," +  i
    str
end Bomb_Tower

case class Boomerang_Tower(id: String, damage: Int, range: Double, attack_rate: Double, traits: Vector[String], pos: GridPos, game: Game) extends Tower(id,damage,range,attack_rate,traits,pos,100,game):
  private var rand = new Random()
  private var upgrade = List("invisibility" -> 100, "Range" -> 100, "attackrate" -> 100, "Damage" -> 150, "Range2" -> 200, "Damage2" -> 300, "attackrate2" -> 200)
  def remove_upgrade(str: String) = upgrade = upgrade.filter(_._1 != str)
  def upgrades = upgrade
  def shoot(): Unit =
    if targets.isDefined then
      val bulletid = rand.nextString(10)
      val bullet = Boomerang(bulletid,Get_Pos,this,targets.get.Get_Pos(),game)
      game.add_bullets(bullet)
      game.apply(Get_Pos).add_Projectile(bullet)
  override def toString: String =
    var str = "Boomerang_Tower," + pos + "," + this.Damage + "," + this.Range + "," + this.Attack_rate + "," + this.Cost
    for i <- Traits do str = str + "," +  i
    str
end Boomerang_Tower

case class Ice_Tower(id: String, damage: Int, range: Double, attack_rate: Double, traits: Vector[String], pos: GridPos, game: Game) extends Tower(id,damage,range,attack_rate,traits,pos,75,game):
  private var rand = new Random()
  private var upgrade = List("invisibility" -> 100, "Range" -> 100, "attackrate" -> 100, "Damage" -> 150, "Range2" -> 200, "Damage2" -> 300, "attackrate2" -> 200)
  def remove_upgrade(str: String) = upgrade = upgrade.filter(_._1 != str)
  def upgrades = upgrade
  def shoot(): Unit =
    if targets.isDefined  then
      val bulletid = rand.nextString(10)
      val bullet = Ice(bulletid,Get_Pos,this,targets.get.Get_Pos(),game)
      game.add_bullets(bullet)
      game.apply(Get_Pos).add_Projectile(bullet)
  override def toString: String =
    var str = "Ice_Tower," + pos + "," + this.Damage + "," + this.Range + "," + this.Attack_rate + "," + this.Cost
    for i <- Traits do str = str + "," +  i
    str
end Ice_Tower

case class Goo_Tower(id: String,  damage: Int, range: Double, attack_rate: Double, traits: Vector[String], pos: GridPos, game: Game) extends Tower(id, damage, range, attack_rate,traits,pos,75,game):
  private var rand = new Random()
  private var upgrade = List("invisibility" -> 100, "Range" -> 100, "attackrate" -> 100, "Damage" -> 150, "Range2" -> 200, "Damage2" -> 300, "attackrate2" -> 200)
  def remove_upgrade(str: String) = upgrade = upgrade.filter(_._1 != str)
  def upgrades = upgrade
  def shoot(): Unit =
    if targets.isDefined then
      val bulletid = rand.nextString(10)
      val bullet = Goo(bulletid,Get_Pos,this,targets.get.Get_Pos(),game)
      game.add_bullets(bullet)
      game.apply(Get_Pos).add_Projectile(bullet)
  override def toString: String =
    var str = "Goo_Tower," + pos + "," + this.Damage + "," + this.Range + "," + this.Attack_rate + "," + this.Cost
    for i <- Traits do str = str + "," +  i
    str
end Goo_Tower
