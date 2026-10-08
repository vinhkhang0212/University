package Classes

import o1.grid.*

abstract class Projectile(id: String, pos: GridPos, tower: Tower, speed: Int, target: GridPos, game: Game):
  var target_position = target
  var position = pos

  def Speed = speed
  
  def Hit(enemy: Enemy): Unit =
    enemy.Get_Hit(this.tower.Damage,this)

  def Move(): Unit =
    game.apply(position).clear(this)
    var dy = this.position.y - target_position.y
    var dx = this.position.x - target_position.x
    if dx != 0 || dy != 0 then
      if (dx > 1 && dy > 1) || (dx > 0 && dy > 1) || (dx > 1 && dy > 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x - 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y - 1)
              else
                game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x - 1, position.y - 2)
                  case m: Path => position = GridPos(position.x - 1, position.y - 2)
            case n: Path =>
              game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x - 1, position.y - 2)
                  case m: Path => position = GridPos(position.x - 1, position.y - 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x - 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y - 1)
              else
                game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y - 1)
                  case m: Path => position = GridPos(position.x - 2, position.y - 1)
            case n: Path =>
              game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y - 1)
                  case m: Path => position = GridPos(position.x - 2, position.y - 1)
        else
          game(GridPos(position.x - 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y - 1)
              else position = GridPos(position.x - 2, position.y - 2)
            case n: Path => position = GridPos(position.x - 2, position.y - 2)
      else if (dx > 1 && dy < -1) || (dx > 0 && dy < -1) || (dx > 1 && dy < 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x - 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y + 1)
              else
                game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x - 1, position.y + 2)
                  case m: Path => position = GridPos(position.x - 1, position.y + 2)
            case n: Path =>
              game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x - 1, position.y + 2)
                  case m: Path => position = GridPos(position.x - 1, position.y + 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x - 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y + 1)
              else
                game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y + 1)
                  case m: Path => position = GridPos(position.x - 2, position.y + 1)
            case n: Path =>
              game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y + 1)
                  case m: Path => position = GridPos(position.x - 2, position.y + 1)
        else
          game(GridPos(position.x - 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y + 1)
              else position = GridPos(position.x - 2, position.y + 2)
            case n: Path => position = GridPos(position.x - 2, position.y + 2)
      else if (dx < -1 && dy > 1) || (dx < 0 && dy > 1) || (dx < -1 && dy > 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x + 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y - 1)
              else
                game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x + 1, position.y - 2)
                  case m: Path => position = GridPos(position.x + 1, position.y - 2)
            case n: Path =>
              game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x + 1, position.y - 2)
                  case m: Path => position = GridPos(position.x + 1, position.y - 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x + 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y - 1)
              else
                game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y - 1)
                  case m: Path => position = GridPos(position.x + 2, position.y - 1)
            case n: Path =>
              game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y - 1)
                  case m: Path => position = GridPos(position.x + 2, position.y - 1)
        else
          game(GridPos(position.x + 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y - 1)
              else position = GridPos(position.x + 2, position.y - 2)
            case n: Path => position = GridPos(position.x + 2, position.y - 2)
      else if (dx < -1 && dy < -1) || (dx < 0 && dy < -1) || (dx < -1 && dy < 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x + 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y + 1)
              else
                game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x + 1, position.y + 2)
                  case m: Path => position = GridPos(position.x + 1, position.y + 2)
            case n: Path =>
              game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x + 1, position.y + 2)
                  case m: Path => position = GridPos(position.x + 1, position.y + 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x + 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y + 1)
              else
                game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y + 1)
                  case m: Path => position = GridPos(position.x + 2, position.y + 1)
            case n: Path =>
              game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y + 1)
                  case m: Path => position = GridPos(position.x + 2, position.y + 1)
        else
          game(GridPos(position.x + 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y + 1)
              else position = GridPos(position.x + 2, position.y + 2)
            case n: Path => position = GridPos(position.x + 2, position.y + 2)
      else if dx == 0 then
        if dy > 1 then 
          game(GridPos(position.x, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
              else position = GridPos(position.x, position.y - 2)
            case n: Path => position = GridPos(position.x, position.y - 2)
        else if dy < -1 then 
          game(GridPos(position.x, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
              else position = GridPos(position.x, position.y + 2)
            case n: Path => position = GridPos(position.x, position.y + 2)
        else position = target_position
      else if dy == 0 then
        if dx > 1 then 
          game(GridPos(position.x - 1, position.y)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
              else position = GridPos(position.x - 2, position.y)
            case n: Path => position = GridPos(position.x - 2, position.y)
        else if dx < -1 then 
          game(GridPos(position.x + 1, position.y)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
              else position = GridPos(position.x + 2, position.y)
            case n: Path => position = GridPos(position.x + 2, position.y)
        else position = target_position
      else
        position = target_position
      game(position) match
        case n: Other =>
          if n.Obstacle.isDefined then
            game.remove_bullets(this)
          else
            game(position).add_Projectile(this)
        case n: Path => game(position).add_Projectile(this)
    else
      game(target_position) match
        case n: Path =>
          if n.enemy.nonEmpty then
            Hit(n.enemy.head)
          game.remove_bullets(this)
        case n: Other => println("Error")

end Projectile

case class Basic_Bullet(id:String, pos: GridPos, t: Tower, e: GridPos, game: Game) extends Projectile(id,pos,t,3,e,game)

case class Ice(id:String, pos: GridPos, t: Tower, e: GridPos, game: Game) extends Projectile(id, pos,t,3,e,game)

case class Fire(id:String, pos: GridPos, t: Tower, e: GridPos, game: Game) extends Projectile(id, pos,t,3,e,game)

case class Boomerang(id:String, pos: GridPos, t: Tower, e: GridPos, game: Game) extends Projectile(id, pos,t,3,e,game):
  private var hit_target = false
  private var tower_pos = t.Get_Pos
  override def Move(): Unit =
    game.apply(position).clear(this)
    
    if !hit_target then
      var dy = this.position.y - target_position.y
      var dx = this.position.x - target_position.x
      if dx > 0 then
        position = GridPos(position.x - 1, position.y)
      else if dx < 0 then
        position = GridPos(position.x + 1, position.y)
      else if dy > 0 then 
        position = GridPos(position.x, position.y - 1)
      else if dy < 0 then 
        position = GridPos(position.x, position.y + 1)
      else
        hit_target = true
      game(position) match
        case n: Other =>
          if n.Obstacle.isDefined then
            game.remove_bullets(this)
          else
            game(position).add_Projectile(this)
        case n: Path => game(position).add_Projectile(this)
    else
      var dy = this.position.y - tower_pos.y
      var dx = this.position.x - tower_pos.x
      if dx == 0 && dy ==0 then 
        game.remove_bullets(this)
      else 
        if dx > 0 then
          position = GridPos(position.x - 1, position.y)
        else if dx < 0 then
          position = GridPos(position.x + 1, position.y)
        else if dy > 0 then 
          position = GridPos(position.x, position.y - 1)
        else 
          position = GridPos(position.x, position.y + 1)
        game(position) match
        case n: Other =>
          if n.Obstacle.isDefined then
            game.remove_bullets(this)
          else
            game(position).add_Projectile(this)
        case n: Path => game(position).add_Projectile(this)
        
    game(position) match
      case n: Path =>
        for enemy <- n.enemy do 
          Hit(enemy)
      case n: Other => val str = "this does nothing"

case class Bomb(id:String, pos: GridPos, t: Tower, e: GridPos, game: Game) extends Projectile(id, pos,t,3,e,game):
  override def Move(): Unit =
    game.apply(position).clear(this)
    var dy = this.position.y - target_position.y
    var dx = this.position.x - target_position.x
    if dx != 0 || dy != 0 then
      if (dx > 1 && dy > 1) || (dx > 0 && dy > 1) || (dx > 1 && dy > 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x - 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y - 1)
              else
                game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x - 1, position.y - 2)
                  case m: Path => position = GridPos(position.x - 1, position.y - 2)
            case n: Path =>
              game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x - 1, position.y - 2)
                  case m: Path => position = GridPos(position.x - 1, position.y - 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x - 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y - 1)
              else
                game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y - 1)
                  case m: Path => position = GridPos(position.x - 2, position.y - 1)
            case n: Path =>
              game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y - 1)
                  case m: Path => position = GridPos(position.x - 2, position.y - 1)
        else
          game(GridPos(position.x - 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y - 1)
              else position = GridPos(position.x - 2, position.y - 2)
            case n: Path => position = GridPos(position.x - 2, position.y - 2)
      else if (dx > 1 && dy < -1) || (dx > 0 && dy < -1) || (dx > 1 && dy < 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x - 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y + 1)
              else
                game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x - 1, position.y + 2)
                  case m: Path => position = GridPos(position.x - 1, position.y + 2)
            case n: Path =>
              game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x - 1, position.y + 2)
                  case m: Path => position = GridPos(position.x - 1, position.y + 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x - 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y + 1)
              else
                game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y + 1)
                  case m: Path => position = GridPos(position.x - 2, position.y + 1)
            case n: Path =>
              game(GridPos(position.x - 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
                    else position = GridPos(position.x - 2, position.y + 1)
                  case m: Path => position = GridPos(position.x - 2, position.y + 1)
        else
          game(GridPos(position.x - 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y + 1)
              else position = GridPos(position.x - 2, position.y + 2)
            case n: Path => position = GridPos(position.x - 2, position.y + 2)
      else if (dx < -1 && dy > 1) || (dx < 0 && dy > 1) || (dx < -1 && dy > 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x + 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y - 1)
              else
                game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x + 1, position.y - 2)
                  case m: Path => position = GridPos(position.x + 1, position.y - 2)
            case n: Path =>
              game(GridPos(position.x, position.y - 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
                    else position = GridPos(position.x + 1, position.y - 2)
                  case m: Path => position = GridPos(position.x + 1, position.y - 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x + 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y - 1)
              else
                game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y - 1)
                  case m: Path => position = GridPos(position.x + 2, position.y - 1)
            case n: Path =>
              game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y - 1)
                  case m: Path => position = GridPos(position.x + 2, position.y - 1)
        else
          game(GridPos(position.x + 1, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y - 1)
              else position = GridPos(position.x + 2, position.y - 2)
            case n: Path => position = GridPos(position.x + 2, position.y - 2)
      else if (dx < -1 && dy < -1) || (dx < 0 && dy < -1) || (dx < -1 && dy < 0) then
        if math.abs(dy) > math.abs(dx) then
          game(GridPos(position.x + 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y + 1)
              else
                game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x + 1, position.y + 2)
                  case m: Path => position = GridPos(position.x + 1, position.y + 2)
            case n: Path =>
              game(GridPos(position.x, position.y + 1)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
                    else position = GridPos(position.x + 1, position.y + 2)
                  case m: Path => position = GridPos(position.x + 1, position.y + 2)
        else if math.abs(dy) < math.abs(dx) then
          game(GridPos(position.x + 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y + 1)
              else
                game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y + 1)
                  case m: Path => position = GridPos(position.x + 2, position.y + 1)
            case n: Path =>
              game(GridPos(position.x + 1, position.y)) match
                  case m: Other =>
                    if m.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
                    else position = GridPos(position.x + 2, position.y + 1)
                  case m: Path => position = GridPos(position.x + 2, position.y + 1)
        else
          game(GridPos(position.x + 1, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y + 1)
              else position = GridPos(position.x + 2, position.y + 2)
            case n: Path => position = GridPos(position.x + 2, position.y + 2)
      else if dx == 0 then
        if dy > 1 then 
          game(GridPos(position.x, position.y - 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x, position.y - 1)
              else position = GridPos(position.x, position.y - 2)
            case n: Path => position = GridPos(position.x, position.y - 2)
        else if dy < -1 then 
          game(GridPos(position.x, position.y + 1)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x, position.y + 1)
              else position = GridPos(position.x, position.y + 2)
            case n: Path => position = GridPos(position.x, position.y + 2)
        else position = target_position
      else if dy == 0 then
        if dx > 1 then 
          game(GridPos(position.x - 1, position.y)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x - 1, position.y)
              else position = GridPos(position.x - 2, position.y)
            case n: Path => position = GridPos(position.x - 2, position.y)
        else if dx < -1 then 
          game(GridPos(position.x + 1, position.y)) match
            case n: Other =>
              if n.Obstacle.isDefined then position = GridPos(position.x + 1, position.y)
              else position = GridPos(position.x + 2, position.y)
            case n: Path => position = GridPos(position.x + 2, position.y)
        else position = target_position
      else
        position = target_position
      game(position) match
        case n: Other =>
          if n.Obstacle.isDefined then
            game.remove_bullets(this)
          else
            game(position).add_Projectile(this)
        case n: Path => game(position).add_Projectile(this)
    else
      for i <- 0 to 2 do
        for j <- 0 to 2 do
          var tilex = target_position.x - 1 + i
          if tilex < 0 then tilex = 0
          else if tilex >= 20 then tilex = 19
          var tiley = target_position.y - 1 + i
          if tiley < 0 then tiley = 0
          else if tiley >= 20 then tiley = 19
          val tile = GridPos(tilex, tiley)
          game(tile) match
            case n: Path =>
              if n.enemy.nonEmpty then
                for enemy <- n.enemy do
                  Hit(n.enemy.head)
            case n: Other => val str = "this does nothing"
      game.remove_bullets(this)

case class Goo(id:String, pos: GridPos, t: Tower, e: GridPos, game: Game) extends Projectile(id, pos,t,3,e,game)
