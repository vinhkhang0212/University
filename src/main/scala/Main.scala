import scalafx.application.JFXApp3
import scalafx.scene.layout.*
import scalafx.scene.{Scene, control, image}
import scalafx.scene.image.*
import scalafx.scene.control.*
import scalafx.geometry.Insets
import scalafx.Includes.*
import scalafx.scene.shape.{Circle,Rectangle}
import scalafx.scene.paint.Color.*
import scalafx.scene.text.*
import scalafx.event.ActionEvent.*
import scalafx.animation.AnimationTimer
import o1.grid.*
import Classes.*
import o1.util.Source

import scala.util.Random
import scalafx.beans.property.{BooleanProperty, IntegerProperty, StringProperty}
import scalafx.scene.image.ImageView
object Main extends JFXApp3:

  var roundsStart = false
  val rand = new Random()
  var difficulty = 1
  var game = Game("Map1",20,20,300,100,1,difficulty)
  val prohibited = List(Red,Blue,Green,Black,Yellow,Pink,LightBlue,Gray, LightYellow)
  var Path_Color = LightGreen
  var Other_Color = White

  def Load_Game: Boolean =
    val saveSource = Source.fromResource("saveData/games.json")
    try
      val save = upickle.default.read[SaveFormat](saveSource.mkString)
      saveSource.close()
      val round = save.data.head.Round
      val money = save.data.head.Money
      val health = save.data.head.Health
      val Map = save.data.head.Map
      difficulty = save.data.head.Difficulty
      game = Game(Map,20,20,money,health,round,difficulty)
      roundsStart = false
      if game.create_Board() then
        val Tower_data = save.data.head.Towers
        for i <- Tower_data do
          val data = i.split(",")
          val pos = GridPos(data(1).filter(_ != '(').toInt, data(2).filter(_ != ')').toInt)
          val damage = data(3).toInt
          val range = data(4).toDouble
          val attack_rate = data(5).toDouble
          val cost = data(6).toInt
          val id = rand.nextString(10)
          var Trait = Vector[String]()
          if data.size > 7 then
            for j <- 7 until data.size do
              Trait = Trait.concat(Seq(data(j)))
          game(pos) match
            case n: Other =>
              data.head match
                case "Basic_Tower" =>
                  game.add_Money(50)
                  game.Place_Tower(Basic_Tower(id,damage,range,attack_rate, Trait, pos,game), pos)
                  n.Tower.get.load_cost(cost)
                  if n.Tower.get.Traits.contains("invisibility") then
                    n.Tower.get.remove_upgrade("invisibility")

                  if n.Tower.get.Traits.contains("Fire") then
                    n.Tower.get.remove_upgrade("Fire")

                  if damage - 5 == 15 then
                    n.Tower.get.remove_upgrade("Damage")
                    n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 5 == 10 then n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 5 == 5 then n.Tower.get.remove_upgrade("Damage")
                  else if damage != 5 then throw Exception()

                  if range - 3 == 3 then
                    n.Tower.get.remove_upgrade("Range")
                    n.Tower.get.remove_upgrade("Range2")
                  else if range - 3 == 2 then n.Tower.get.remove_upgrade("Range2")
                  else if range - 3 == 1 then n.Tower.get.remove_upgrade("Range")
                  else if range != 3 then throw Exception()

                  if 10 - attack_rate == 3 then
                    n.Tower.get.remove_upgrade("attackrate")
                    n.Tower.get.remove_upgrade("attackrate2")
                  else if 10 - attack_rate == 2 then n.Tower.get.remove_upgrade("attackrate2")
                  else if 10 - attack_rate == 1 then n.Tower.get.remove_upgrade("attackrate")
                  else if attack_rate != 10 then throw Exception()

                case "Boomerang_Tower" =>
                  game.add_Money(100)
                  game.Place_Tower(Boomerang_Tower(id,damage,range,attack_rate, Trait, pos,game), pos)
                  n.Tower.get.load_cost(cost)
                  if n.Tower.get.Traits.contains("invisibility") then
                    n.Tower.get.remove_upgrade("invisibility")

                  if damage - 7 == 15 then
                    n.Tower.get.remove_upgrade("Damage")
                    n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 7 == 10 then n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 7 == 5 then n.Tower.get.remove_upgrade("Damage")
                  else if damage != 7 then throw Exception()

                  if range - 5 == 3 then
                    n.Tower.get.remove_upgrade("Range")
                    n.Tower.get.remove_upgrade("Range2")
                  else if range - 5 == 2 then n.Tower.get.remove_upgrade("Range2")
                  else if range - 5 == 1 then n.Tower.get.remove_upgrade("Range")
                  else if range != 5 then throw Exception()

                  if 15 - attack_rate == 3 then
                    n.Tower.get.remove_upgrade("attackrate")
                    n.Tower.get.remove_upgrade("attackrate2")
                  else if 15 - attack_rate == 2 then n.Tower.get.remove_upgrade("attackrate2")
                  else if 15 - attack_rate == 1 then n.Tower.get.remove_upgrade("attackrate")
                  else if attack_rate != 15 then throw Exception()

                case "Bomb_Tower" =>
                  game.add_Money(200)
                  game.Place_Tower(Bomb_Tower(id,damage,range,attack_rate, Trait, pos,game), pos)
                  n.Tower.get.load_cost(cost)
                  if n.Tower.get.Traits.contains("invisibility") then
                    n.Tower.get.remove_upgrade("invisibility")

                  if damage - 15 == 15 then
                    n.Tower.get.remove_upgrade("Damage")
                    n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 15 == 10 then n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 15 == 5 then n.Tower.get.remove_upgrade("Damage")
                  else if damage != 15 then throw Exception()

                  if range - 2 == 3 then
                    n.Tower.get.remove_upgrade("Range")
                    n.Tower.get.remove_upgrade("Range2")
                  else if range - 2 == 2 then n.Tower.get.remove_upgrade("Range2")
                  else if range - 2 == 1 then n.Tower.get.remove_upgrade("Range")
                  else if range != 2 then throw Exception()

                  if 20 - attack_rate == 3 then
                    n.Tower.get.remove_upgrade("attackrate")
                    n.Tower.get.remove_upgrade("attackrate2")
                  else if 20 - attack_rate == 2 then n.Tower.get.remove_upgrade("attackrate2")
                  else if 20 - attack_rate == 1 then n.Tower.get.remove_upgrade("attackrate")
                  else if attack_rate != 20 then throw Exception()

                case "Ice_Tower" =>
                  game.add_Money(75)
                  game.Place_Tower(Ice_Tower(id,damage,range,attack_rate, Trait, pos,game), pos)
                  n.Tower.get.load_cost(cost)
                  if n.Tower.get.Traits.contains("invisibility") then
                    n.Tower.get.remove_upgrade("invisibility")

                  if damage - 2 == 15 then
                    n.Tower.get.remove_upgrade("Damage")
                    n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 2 == 10 then n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 2 == 5 then n.Tower.get.remove_upgrade("Damage")
                  else if damage != 2 then throw Exception()

                  if range - 4 == 3 then
                    n.Tower.get.remove_upgrade("Range")
                    n.Tower.get.remove_upgrade("Range2")
                  else if range - 4 == 2 then n.Tower.get.remove_upgrade("Range2")
                  else if range - 4 == 1 then n.Tower.get.remove_upgrade("Range")
                  else if range != 4 then throw Exception()

                  if 10 - attack_rate == 3 then
                    n.Tower.get.remove_upgrade("attackrate")
                    n.Tower.get.remove_upgrade("attackrate2")
                  else if 10 - attack_rate == 2 then n.Tower.get.remove_upgrade("attackrate2")
                  else if 10 - attack_rate == 1 then n.Tower.get.remove_upgrade("attackrate")
                  else if attack_rate != 10 then throw Exception()

                case "Goo_Tower" =>
                  game.add_Money(75)
                  game.Place_Tower(Goo_Tower(id,damage,range,attack_rate, Trait, pos,game), pos)
                  n.Tower.get.load_cost(cost)
                  if n.Tower.get.Traits.contains("invisibility") then
                    n.Tower.get.remove_upgrade("invisibility")

                  if damage - 2 == 15 then
                    n.Tower.get.remove_upgrade("Damage")
                    n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 2 == 10 then n.Tower.get.remove_upgrade("Damage2")
                  else if damage - 2 == 5 then n.Tower.get.remove_upgrade("Damage")
                  else if damage != 2 then throw Exception()

                  if range - 4 == 3 then
                    n.Tower.get.remove_upgrade("Range")
                    n.Tower.get.remove_upgrade("Range2")
                  else if range - 4 == 2 then n.Tower.get.remove_upgrade("Range2")
                  else if range - 4 == 1 then n.Tower.get.remove_upgrade("Range")
                  else if range != 4 then throw Exception()

                  if 10 - attack_rate == 3 then
                    n.Tower.get.remove_upgrade("attackrate")
                    n.Tower.get.remove_upgrade("attackrate2")
                  else if 10 - attack_rate == 2 then n.Tower.get.remove_upgrade("attackrate2")
                  else if 10 - attack_rate == 1 then n.Tower.get.remove_upgrade("attackrate")
                  else if attack_rate != 10 then throw Exception()
            case n: Path => throw Exception()
      else throw Exception()
      true
    catch
      case _ =>
        println("Error reading file")
        false
  end Load_Game

  def start() =

    stage = new JFXApp3.PrimaryStage:
      title = "Tower defense"
      width = 500
      height = 580
      resizable = false

    stage.setY(10)
    stage.setX(450)

    val view = new Scene()
    stage.scene = view
    Start_Menu(view, stage)
  end start

  def Start_Menu(view: Scene, stage: JFXApp3.PrimaryStage) =

    val label = new Label("Choose a map"):
      font = new Font(50)
      layoutX = 250
      layoutY = 50

    val map1 = new ImageView(Image(getClass.getResourceAsStream("/Images/Map1.png"))):
      layoutX = 20
      layoutY = 150
      fitHeight = 150
      fitWidth = 230

    val button_map1 = new Button("Map1"):
      layoutX = 110
      layoutY = 320
      onMouseClicked = (event) =>
        game = Game("Map1",20,20,300,100,1,difficulty)
        roundsStart = false
        if game.create_Board() then
          view.root = game_Window(game,view, stage)

    val map2 = new ImageView(Image(getClass.getResourceAsStream("/Images/Map2.png"))):
      layoutX = 280
      layoutY = 150
      fitHeight = 150
      fitWidth = 230

    val button_map2 = new Button("Map2"):
      layoutX = 370
      layoutY = 320
      onMouseClicked = (event) =>
        game = Game("Map2",20,20,300,100,1,difficulty)
        roundsStart = false
        if game.create_Board() then
          view.root = game_Window(game,view, stage)

    val map3 = new ImageView(Image(getClass.getResourceAsStream("/Images/Map3.png"))):
      layoutX = 540
      layoutY = 150
      fitHeight = 150
      fitWidth = 230

    val button_map3 = new Button("Map3"):
      layoutX = 630
      layoutY = 320
      onMouseClicked = (event) =>
        game = Game("Map3",20,20,300,100,1,difficulty)
        roundsStart = false
        if game.create_Board() then
          view.root = game_Window(game,view, stage)

    val map4 = new ImageView(Image(getClass.getResourceAsStream("/Images/Map4.png"))):
      layoutX = 150
      layoutY = 400
      fitHeight = 150
      fitWidth = 230

    val button_map4 = new Button("Map4"):
      layoutX = 240
      layoutY = 570
      onMouseClicked = (event) =>
        game = Game("Map4",20,20,300,100,1,difficulty)
        roundsStart = false
        if game.create_Board() then
          view.root = game_Window(game,view, stage)

    val map5 = new ImageView(Image(getClass.getResourceAsStream("/Images/Map5.png"))):
      layoutX = 410
      layoutY = 400
      fitHeight = 150
      fitWidth = 230

    val button_map5 = new Button("Map5"):
      layoutX = 500
      layoutY = 570
      onMouseClicked = (event) =>
        game = Game("Map5",20,20,300,100,1,difficulty)
        roundsStart = false
        if game.create_Board() then
          view.root = game_Window(game,view, stage)

    val view1 = new Pane:
      background = Background.fill(LightGray)
      children = List(label,map1,map2,map3,map4,map5,button_map1, button_map2, button_map3, button_map4,button_map5)

    val label_difficulty = new Label("Choose a difficulty"):
      font = new Font(50)
      layoutX = 40
      layoutY = 50

    val Easy = new Button("Easy"):
      layoutX = 200
      layoutY = 150
      onMouseClicked = (event) =>
        stage.height = 790
        stage.width = 801
        difficulty = 1
        view.root = view1

    val Medium = new Button("Medium"):
      layoutX = 200
      layoutY = 190
      onMouseClicked = (event) =>
        stage.height = 790
        stage.width = 801
        difficulty = 2
        view.root = view1

    val Hard = new Button("Hard"):
      layoutX = 200
      layoutY = 230
      onMouseClicked = (event) =>
        stage.height = 790
        stage.width = 801
        difficulty = 3
        view.root = view1

    val view2 = new Pane:
      children = List(label_difficulty,Easy,Medium,Hard)

    val labelstart = new Label("Tower Defense"):
      font = new Font(50)
      layoutX = 80
      layoutY = 50

    val new_game = new Button("New Game"):
      layoutX = 200
      layoutY = 150
      onMouseClicked = (event) =>
        stage.height = 350
        stage.width = 500
        view.root = view2

    val load_game = new Button("Load Game"):
      layoutX = 200
      layoutY = 200
      onMouseClicked = (event) =>
        if Load_Game then
          stage.height = 790
          stage.width = 801
          view.root = game_Window(game, view, stage)

    val quit_game = new Button("Quit"):
      layoutX = 220
      layoutY = 250
      onMouseClicked = (event) => stage.close()

    val Path_label = new Label("Path color:"):
      layoutX = 180
      layoutY = 300

    val Path_picker = new ColorPicker(Path_Color):
      layoutX = 180
      layoutY = 320
      onAction = (event) =>
        if prohibited.contains(value.apply()) || Other_Color == value.apply() then
          value = Path_Color
        else
          Path_Color = value.apply()


    val Other_label = new Label("Other color:"):
      layoutX = 180
      layoutY = 350

    val Other_picker = new ColorPicker(Other_Color):
      layoutX = 180
      layoutY = 370
      onAction = (event) =>
        if prohibited.contains(value.apply()) || Path_Color == value.apply() then
          value = Other_Color
        else
          Other_Color = value.apply()

    val note_label = new Label("Note: You can not choose these colors: Red, Blue, Green, Yellow, Gray, Black, Pink, LightBlue, LightYellow. Also, Path_Color and Other_Color can not be the same."):
      layoutX = 50
      layoutY = 420
      prefWidth = 400
      wrapText = true

    val viewStart = new Pane:
      children = List(labelstart,new_game,load_game, quit_game, Path_label, Path_picker, Other_label, Other_picker, note_label)

    val back_button1 = new Button("Back"):
      font = new Font(30)
      layoutX = 650
      layoutY = 650
      onMouseClicked = (event) =>
        stage.height = 350
        stage.width = 500
        view.root = view2

    val back_button2 = new Button("Back"):
      layoutX = 200
      layoutY = 270
      onMouseClicked = (event) =>
        stage.height = 580
        stage.width = 500
        view.root = viewStart

    view1.children += back_button1
    view2.children += back_button2

    view.root = viewStart
  end Start_Menu

  def game_Over(view: Scene, stage: JFXApp3.PrimaryStage): Unit =

    stage.width = 500
    stage.height = 300

    val label = new Label("Game Over"):
      font = new Font(50)
      layoutX = 120
      layoutY = 50

    val retry = new Button("Retry"):
      layoutX = 220
      layoutY = 150
      onMouseClicked = (event) =>
        stage.height = 790
        stage.width = 801
        val map = game.current_Map
        game = Game(map,20,20,300,100,1,difficulty)
        roundsStart = false
        game.create_Board()
        view.root = game_Window(game, view, stage)

    val quit_game = new Button("Quit"):
        layoutX = 220
        layoutY = 200
        onMouseClicked = (event) => stage.close()

    val scene = new Pane():
      children = List(label, retry, quit_game)

    view.root = scene
  end game_Over

  def game_Window(game: Game, view: Scene, stage: JFXApp3.PrimaryStage): GridPane =
    var grid_chosen: Option[GridPos] = None
    var chosen_change = false
    val RoundText = new Label("Round: " + game.current_Round):
      background = Background.fill(White)
    val MoneyText = new Label("Money: " + game.Money):
      background = Background.fill(White)
    val HealthText = new Label("Health: " + game.Health):
      background = Background.fill(White)
    val informtationText = new Label("Hello"):
      prefHeight = 140
      prefWidth = 650
      background = Background.fill(White)
      wrapText = true

    val BasicButton = new ToggleButton("Basic Tower: " + (50 * difficulty))

    val BoomerangButton = new ToggleButton("Boomerang Tower: " + (100 * difficulty))

    val BombButton = new ToggleButton("Bomb Tower: " + (200 * difficulty))

    val IceButton = new ToggleButton("Ice Tower: " + (75 * difficulty))

    val GooButton = new ToggleButton("Goo Tower: " + (75 * difficulty))

    val quit_game = new Button("Quit"):
        onMouseClicked = (event) => stage.close()

    val StartButton = new Button("Start round"):
      onMouseClicked = (event) =>
        game.Start_Round()
        roundsStart = true

    val Button1 = new Button("Button 1"):
      onMouseClicked = (event) =>
        if grid_chosen.isDefined then
          game(grid_chosen.get) match
            case n: Other =>
              if n.Tower.isDefined then
                val split = text.apply().split(" ")
                val cost = split(1).toInt
                if game.Money >= cost then
                  game.remove_Money(cost)
                  n.Tower.get.add_Cost(cost)
                  if split(0) == "invisibility" then
                    n.Tower.get.add_Trait("invisibility")
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Fire" then
                    n.Tower.get.add_Trait("Fire")
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Damage" then
                    n.Tower.get.Upgrade_damage(5)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Damage2" then
                    n.Tower.get.Upgrade_damage(10)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Range" then
                    n.Tower.get.Upgrade_range(1)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Range2" then
                    n.Tower.get.Upgrade_range(2)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "attackrate" then
                    n.Tower.get.Upgrade_attacck_rate(1)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "attackrate2" then
                    n.Tower.get.Upgrade_attacck_rate(2)
                    n.Tower.get.remove_upgrade(split(0))
                  chosen_change = true
            case n: Path => println("Error: the square should not be a Path.")

    val Button2 = new Button("Button 2"):
      onMouseClicked = (event) =>
        if grid_chosen.isDefined then
          game(grid_chosen.get) match
            case n: Other =>
              if n.Tower.isDefined then
                val split = text.apply().split(" ")
                val cost = split(1).toInt
                if game.Money >= cost then
                  game.remove_Money(cost)
                  n.Tower.get.add_Cost(cost)
                  if split(0) == "invisibility" then
                    n.Tower.get.add_Trait("invisibility")
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Fire" then
                    n.Tower.get.add_Trait("Fire")
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Damage" then
                    n.Tower.get.Upgrade_damage(5)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Damage2" then
                    n.Tower.get.Upgrade_damage(10)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Range" then
                    n.Tower.get.Upgrade_range(1)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Range2" then
                    n.Tower.get.Upgrade_range(2)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "attackrate" then
                    n.Tower.get.Upgrade_attacck_rate(1)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "attackrate2" then
                    n.Tower.get.Upgrade_attacck_rate(1)
                    n.Tower.get.remove_upgrade(split(0))
                  chosen_change = true
            case n: Path => println("Error: the square should not be a Path.")

    val Button3 = new Button("Button 3"):
      onMouseClicked = (event) =>
        if grid_chosen.isDefined then
          game(grid_chosen.get) match
            case n: Other =>
              if n.Tower.isDefined then
                val split = text.apply().split(" ")
                val cost = split(1).toInt
                if game.Money >= cost then
                  game.remove_Money(cost)
                  n.Tower.get.add_Cost(cost)
                  if split(0) == "invisibility" then
                    n.Tower.get.add_Trait("invisibility")
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Fire" then
                    n.Tower.get.add_Trait("Fire")
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Damage" then
                    n.Tower.get.Upgrade_damage(5)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Damage2" then
                    n.Tower.get.Upgrade_damage(10)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Range" then
                    n.Tower.get.Upgrade_range(1)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "Range2" then
                    n.Tower.get.Upgrade_range(2)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "attackrate" then
                    n.Tower.get.Upgrade_attacck_rate(1)
                    n.Tower.get.remove_upgrade(split(0))
                  else if split(0) == "attackrate2" then
                    n.Tower.get.Upgrade_attacck_rate(1)
                    n.Tower.get.remove_upgrade(split(0))
                  chosen_change = true
            case n: Path => println("Error: the square should not be a Path.")

    val SellButton = new Button("Sell"):
      onMouseClicked = (event) =>
        if grid_chosen.isDefined then
          game(grid_chosen.get) match
            case n: Other =>
              if n.Tower.isDefined then
                game.Sell_Tower(n.Tower.get, n.Tower.get.Get_Pos)
                chosen_change = true
            case n: Path => println("Error: the square should not be a path")

    val SaveButton = new Button("Save"):
      onMouseClicked = (event) => game.Save_Game()

    val x1Button = new RadioButton("x1"):
      background = Background.fill(Gray)
    val x2Button = new RadioButton("x2"):
      background = Background.fill(Gray)
    val radioGroup = new ToggleGroup:
      toggles = List(x1Button,x2Button)
    x1Button.setSelected(true)

    val upgradeBox = new VBox:
      prefHeight = 140
      prefWidth = 200
      spacing = 5
      background = Background.fill(Black)
      children = List()

    val menu = new VBox:
      padding = Insets.apply(10, 10, 10, 10)
      spacing = 15
      prefHeight = 600
      prefWidth = 200
      background = Background.fill(Gray)
      children = List(RoundText, MoneyText, HealthText, BasicButton, BoomerangButton, BombButton, IceButton, GooButton, StartButton, SaveButton, quit_game, x1Button, x2Button)

    val TowersToggle = new ToggleGroup():
      toggles = List(BasicButton, BombButton, BoomerangButton, IceButton, GooButton)

    var initial = List[StackPane]()
    for i <- 0 until 20 do
      for j <- 0 until 20 do
        game(GridPos(i, j)) match
          case n: Other =>
            val rec = new StackPane():
              background = Background.fill(Other_Color)
              prefHeight = 30
              prefWidth = 30
            initial = initial.concat(List(rec))
          case n: Path =>
            val rec = new StackPane():
              background = Background.fill(Path_Color)
              prefHeight = 30
              prefWidth = 30
            initial = initial.concat(List(rec))

    for num <- initial.indices do
      var i = num / 20
      var j = num - i * 20
      game(GridPos(i, j)) match
        case n: Other =>
          initial(num).onMouseEntered = (event) =>
            if n.Tower.isDefined || n.Obstacle.isDefined then initial(num).background = Background.fill(Red)
            else if TowersToggle.toggles.exists(_.isSelected) then
              var r = 0
              if BasicButton.isSelected then r = 3
              if BoomerangButton.isSelected then r = 4
              if BombButton.isSelected then r = 2
              if IceButton.isSelected then r = 4
              if GooButton.isSelected then r = 4
              for i1 <- math.max(i - r, 0) to math.min(i + r, 19) do
                for j1 <- math.max(j - r, 0) to math.min(j + r, 19) do
                  game(GridPos(i1, j1)) match
                    case m: Other => initial(i1 * 20 + j1).background = Background.fill(Gray)
                    case m: Path => initial(i1 * 20 + j1).background = Background.fill(LightBlue)
              initial(num).background = Background.fill(LightYellow)
            else initial(num).background = Background.fill(Gray)

          initial(num).onMouseExited = (event) =>
            if TowersToggle.toggles.exists(_.isSelected) then
              var r = 0
              if BasicButton.isSelected then r = 3
              if BoomerangButton.isSelected then r = 4
              if BombButton.isSelected then r = 2
              if IceButton.isSelected then r = 4
              if GooButton.isSelected then r = 4
              for i1 <- math.max(i - r, 0) to math.min(i + r, 19) do
                for j1 <- math.max(j - r, 0) to math.min(j + r, 19) do
                  game(GridPos(i1, j1)) match
                    case m: Other => initial(i1 * 20 + j1).background = Background.fill(Other_Color)
                    case m: Path => initial(i1 * 20 + j1).background = Background.fill(Path_Color)
            else initial(num).background = Background.fill(Other_Color)

          initial(num).onMouseClicked = (event) =>
            grid_chosen = Option(GridPos(num / 20, num % 20))
            chosen_change = true
            if n.Tower.isEmpty && n.Obstacle.isEmpty then
              val towerid = rand.nextString(10)
              if BasicButton.isSelected then
                if game.Place_Tower(Basic_Tower(towerid, 5, 3, 10, Vector(), GridPos(i, j), game), GridPos(i, j)) then
                  var tower = Circle(15, Red)
                  initial(num).children = tower
              if BoomerangButton.isSelected then
                if game.Place_Tower(Boomerang_Tower(towerid, 7, 5, 15, Vector(), GridPos(i, j), game), GridPos(i, j)) then
                  var tower = Circle(15, Green)
                  initial(num).children = tower
              if BombButton.isSelected then
                if game.Place_Tower(Bomb_Tower(towerid, 15, 2, 20, Vector("Fire"), GridPos(i, j), game), GridPos(i, j)) then
                  var tower = Circle(15, Black)
                  initial(num).children = tower
              if IceButton.isSelected then
                if game.Place_Tower(Ice_Tower(towerid, 2, 4, 10, Vector("Ice"), GridPos(i, j), game), GridPos(i, j)) then
                  var tower = Circle(15, Blue)
                  initial(num).children = tower
              if GooButton.isSelected then
                if game.Place_Tower(Goo_Tower(towerid, 2, 4, 10, Vector("Goo"), GridPos(i, j), game), GridPos(i, j)) then
                  var tower = Circle(15, Yellow)
                  initial(num).children = tower
        case n: Path =>
          initial(num).onMouseEntered = (event) =>
            if TowersToggle.toggles.exists(_.isSelected) then initial(num).background = Background.fill(Red)
            else initial(num).background = Background.fill(LightBlue)
          initial(num).onMouseExited = (event) => initial(num).background = Background.fill(Path_Color)

    val window = new TilePane():
      prefHeight = 630
      prefWidth = 650
      background = Background.fill(Black)
      children = initial

    val gameWin = GridPane()

    gameWin.add(window, 0, 0)
    gameWin.add(menu, 1, 0)
    gameWin.add(informtationText, 0, 1)
    gameWin.add(upgradeBox, 1, 1)

    var roundStart = false

    def displayEnemy(m: Path, i: Int, j: Int) =
      for enemy <- m.enemy do
        enemy match
          case n: Basic =>
            var circle = Rectangle(5, 5, Black)
            if n.traits.contains("Invisible") then
              circle = Rectangle(5, 5, Grey)
            initial(i * 20 + j).children += circle
          case n: Fast =>
            var circle = Circle(5, Blue)
            if n.traits.contains("Invisible") then
              circle = Circle(5, LightBlue)
            initial(i * 20 + j).children += circle
          case n: Big =>
            var circle = Rectangle(10, 10, Red)
            if n.traits.contains("Invisible") then
              circle = Rectangle(10, 10, Pink)
            initial(i * 20 + j).children += circle
          case n: Faster =>
            var circle = Circle(10, Blue)
            if n.traits.contains("Invisible") then
              circle = Circle(10, LightBlue)
            initial(i * 20 + j).children += circle
          case n: Bigger =>
            var circle = Rectangle(20, 20, Red)
            if n.traits.contains("Invisible") then
              circle = Rectangle(20, 20, Pink)
            initial(i * 20 + j).children += circle
          case n: Carrier =>
            var circle = Rectangle(20, 20, Black)
            if n.traits.contains("Invisible") then
              circle = Rectangle(20, 20, Gray)
            initial(i * 20 + j).children += circle

    def displayBullet(m: Square, i: Int, j: Int) =
      for bullet <- m.bullets() do
        bullet match
          case n: Basic_Bullet =>
            var circle = Circle(2, Black)
            initial(i * 20 + j).children += circle
          case n: Bomb =>
            var circle = Circle(5, Black)
            initial(i * 20 + j).children += circle
          case n: Boomerang =>
            var circle = Circle(3, Green)
            initial(i * 20 + j).children += circle
          case n: Fire =>
            var circle = Circle(2, Red)
            initial(i * 20 + j).children += circle
          case n: Ice =>
            var circle = Circle(2, Blue)
            initial(i * 20 + j).children += circle
          case n: Goo =>
            var circle = Circle(2, Gold)
            initial(i * 20 + j).children += circle

    def displayTower(m: Other, i: Int, j: Int) =
      m.Tower.get match
        case n: Basic_Tower =>
          var tower = Circle(10, Red)
          initial(i * 20 + j).children += tower
        case n: Boomerang_Tower =>
          var tower = Circle(15, Green)
          initial(i * 20 + j).children += tower
        case n: Bomb_Tower =>
          var tower = Rectangle(10,10, Black)
          initial(i * 20 + j).children += tower
        case n: Ice_Tower =>
          var tower = Circle(10, Blue)
          initial(i * 20 + j).children += tower
        case n: Goo_Tower =>
          var tower = Circle(10, Yellow)
          initial(i * 20 + j).children += tower
          
    def displayObstacle(m: Other, i: Int, j: Int) =
      if m.Obstacle.isDefined then
        var obstacle = Rectangle(30,30,Black)
        initial(i * 20 + j).children += obstacle 

    def tick() =
      var str = "Welcome to the game. Your mission is to prevent the enemies from getting to the end(the square with the red square). Basic enemies are little black squares."
      if game.current_Round > 50 then str = "There will now be Carrier(Black Rectangles) enemy that will spawn more enemy when it dies."
      else if game.current_Round > 30 then str = "Enemies now can have two traits."
      else if game.current_Round > 20 then str = "There will now be Faster enemy(Bigger blue circle) who moves even faster and Bigger enemy(Bigger red ractangles) who will have even more health."
      else if game.current_Round > 10 then str = "Some enemies will have a trait. Invisible will make enemies invisible(lighter color) to normal tower. Tower will need invisibility trait to see invisible enemy. Fire and Ice immunity will make enemies immune to Fire and Ice traits"
      else if game.current_Round > 3 then str = "Fast and Big enemies will now appear. Fast enemies are blue circles and Big enemies are red rectangles. Fast enemies are faster and Big enemies have more health"
      HealthText.text = "Health: " + game.Health
      MoneyText.text = "Money: " + game.Money
      RoundText.text = "Round: " + game.current_Round
      informtationText.text = str
      if grid_chosen.isDefined then
        game(grid_chosen.get) match
          case n: Other =>
            if n.Tower.isDefined then
              var alternatestr = ""
              val tower = n.Tower.get
              tower match
                case m: Basic_Tower => alternatestr = alternatestr + "Basic Tower"
                case m: Boomerang_Tower => alternatestr = alternatestr + "Boomerang Tower"
                case m: Bomb_Tower => alternatestr = alternatestr + "Bomb Tower"
                case m: Ice_Tower => alternatestr = alternatestr + "Ice_Tower"
                case m: Goo_Tower => alternatestr = alternatestr + "Goo_Tower"
              alternatestr = alternatestr + " damage: " + tower.Damage
              alternatestr = alternatestr + " range: " + tower.Range
              alternatestr = alternatestr + " attack rate: " + tower.Attack_rate
              alternatestr = alternatestr + " Trait: ["
              for Trait <- tower.Traits do
                alternatestr = alternatestr + " " + Trait
              alternatestr = alternatestr + " ]"
              informtationText.text = alternatestr
              if chosen_change then
                chosen_change = false
                upgradeBox.children = List()
                var upgrade = List[(String, Int)]()
                if tower.upgrades.size > 3 then upgrade = tower.upgrades.take(3)
                else upgrade = tower.upgrades.take(tower.upgrades.size)
                var cnt = 0
                for i <- upgrade do
                  cnt = cnt + 1
                  val text = i._1 + " " + i._2 * difficulty
                  if cnt == 1 then
                    Button1.text = text
                    upgradeBox.children += Button1
                  else if cnt == 2 then
                    Button2.text = text
                    upgradeBox.children += Button2
                  else if cnt == 3 then
                    Button3.text = text
                    upgradeBox.children += Button3
                upgradeBox.children += SellButton
            else
              chosen_change = false
              upgradeBox.children = List()
          case n: Path => println("Error: the square should not be a Path.")

      for i <- 0 until 20 do
        for j <- 0 until 20 do
          game(GridPos(i, j)) match
            case m: Path =>
              initial(i * 20 + j).children = List()
              if GridPos(i,j) == game.path.last then
                val rectangle = Rectangle(20,20,Red)
                initial(i * 20 + j).children += rectangle

              if m.enemy.nonEmpty then displayEnemy(m, i, j)
              if m.bullets().nonEmpty then displayBullet(m, i, j)
            case m: Other =>
              initial(i * 20 + j).children = List()
              if m.Tower.isDefined then displayTower(m, i, j)
              displayObstacle(m, i, j)
              if m.bullets().nonEmpty then displayBullet(m, i, j)
              
      window.children = initial
      if roundsStart then
        if game.Enemy.nonEmpty || game.bullets.nonEmpty || game.currentEnemy.nonEmpty then
          game.tick()
        else
          game.tick()
          roundsStart = false
    end tick

    var timer = AnimationTimer(_ => tick())
    timer.start()

    x1Button.selected.onChange((_, _, newValue) =>
      val value = newValue.booleanValue()
      if value then
        timer.stop()
        timer = AnimationTimer(_ => tick())
        timer.start()
      else
        timer.stop()
        timer = AnimationTimer(_ =>
          tick()
          tick()
        )
        timer.start()
    )

    game.health.onChange((_, _, newValue) =>
      val health = newValue.intValue()
      if health <= 0 then
        roundsStart = false
        timer.stop()
        game_Over(view, stage)
    )

    gameWin
  end game_Window

end Main

