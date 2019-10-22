class Programmer{
  def CreateApp(face: Facade){
    face.Start
    face.Stop
  }
}

class Facade(te: TextEditor, com: Compiler, cle: CLR){
  var textEditor: TextEditor = te
  var compiler: Compiler = com
  var clr: CLR = cle

  def Start
  {
    textEditor.CreateCode
    textEditor.Save
    compiler.Compile
    clr.Execute
  }
  def Stop
  {
    clr.Finish
  }
}

class CLR{
  def Execute{
    println("Выполнение приложения");
  }
  def Finish{
    println("Завершение работы приложения");
  }
}

class Compiler{

  def Compile{
    println("Компиляция приложения")
  }
}

class TextEditor{

  def CreateCode{
    println("Написание кода")
  }

  def Save(){
    println("Сохранение кода")
  }
}

object Run {
  def main(args: Array[String]): Unit ={
    val textEditor: TextEditor = new TextEditor
    val compiler: Compiler = new Compiler
    val clr: CLR = new CLR

    val ide: Facade = new Facade(textEditor,compiler,clr)

    val programmer: Programmer = new Programmer
    programmer.CreateApp(ide)
  }
}