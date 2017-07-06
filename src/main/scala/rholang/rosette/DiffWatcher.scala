package coop.rchain.rho2rose

import java.nio.file._

import scala.collection.JavaConversions._

object DiffWatcher {
	private def watchFileAndCallBack(fileName:String, onChange:() => Unit):Unit = {
		val path = FileSystems.getDefault().getPath(fileName)
		if(path.toFile().exists()){
			val dir = path.getParent()
			val watchService = FileSystems.getDefault().newWatchService()
			dir.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY)
			while (true) {
				print("Watching... ")
				val watchKey = watchService.take()
				println("changed!")
				for(event <- watchKey.pollEvents().toList){
					val changed = event.context().asInstanceOf[Path]
					if(changed.getFileName() == path.getFileName()){
						onChange()
					}
				}
			}
		} else println(s"${fileName} doesn't exist")
	}
	def indent(ast : VisitorTypes.R):String = {
		var indent = ""
		Rholang2RosetteCompiler.serialize(ast).map( (c) => { 
			c match { 
				case '(' => {
					indent = indent + "  ";
					"\n" + indent + "(";
				} 
				case ')' => {
					indent = indent.substring(0,indent.length-2);
					c
				} 
				case _ => c 
			} 
		} ).mkString("")
	}
	def watch(file:String): Unit = {
		var text1 = indent(Rholang2RosetteCompiler.compile(file))
		watchFileAndCallBack(file,() => {
			val text2 = indent(Rholang2RosetteCompiler.compile(file))
			println("-----------------------")
			println(text1)
			println("-----------------------")
			println(text2)
			println("-----------------------")
			text1 = text2;
		});
	}
}