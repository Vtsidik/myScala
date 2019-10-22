import scala.collection.mutable.Stack

trait ICommand{
  def Execute
  def Undo
}

class MultiPult
{

  var buttons = new Array[ICommand](2)
  var commandHistory = Stack[ICommand]()

  def MultiPult{
    for(i <- 0 to buttons.length - 1){
      buttons(i) = new NoCommand
    }
  }

  def SetCommand(number: Int, com: ICommand){
    buttons(number) = com
  }

  def PressButton(number: Int){
    buttons(number).Execute
    commandHistory.push(buttons(number))
  }

  def PressUndoButton{
    if(commandHistory.size > 0){
      var undoCommand: ICommand = commandHistory.pop
      undoCommand.Undo
    }
  }
}

class NoCommand extends ICommand{
  def Execute{}
  def Undo{}
}

class TVOnCommand(tvSet: TV) extends ICommand{

  var tv: TV = tvSet

  def Execute{
    tv.On
  }

  def Undo{
    tv.Off
  }
}

class VolumeCommand(v: Volume) extends ICommand{

  var volume: Volume = v

  def Execute{
    volume.RaiseLevel
  }

  def Undo{
    volume.DropLevel
  }
}

class TV{

  def On{
    println("Телевизор включен!")
  }

  def Off{
    println("Телевизор выключен...")
  }
}

class Volume{

  val OFF: Int = 0
  val HIGH: Int = 20
  var level: Int = 0

  def Volume{
    level = OFF
  }

  def RaiseLevel{
    if(level < HIGH) level += 1
    println("Уровень звука " + level)
  }

  def DropLevel{
    if(level > OFF) level -= 1
    println("Уровень звука " + level)
  }
}

object Run {
  def main(args: Array[String]): Unit ={
    var tv: TV = new TV
    var volume: Volume = new Volume
    var mPult: MultiPult = new MultiPult
    mPult.SetCommand(0, new TVOnCommand(tv))
    mPult.SetCommand(1, new VolumeCommand(volume))

    mPult.PressButton(0)
    mPult.PressButton(1)
    mPult.PressButton(1)
    mPult.PressButton(1)

    mPult.PressUndoButton
    mPult.PressUndoButton
    mPult.PressUndoButton
    mPult.PressUndoButton
  }
}