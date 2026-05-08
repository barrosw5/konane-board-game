object saveRandom {
    
    def loadRandomFromFile(filename: String): MyRandom = {
        
        // 1. Tentar ler o ficheiro (Imperativo, mas guardamos o resultado num Option, igual ao que fizeste no Main)
        val fileContentOpt: Option[String] = try {
            val source = scala.io.Source.fromFile(filename)
            val content = source.mkString.trim // O trim remove espaços ou quebras de linha a mais
            source.close()
            Some(content)
        } catch {
            case _: Exception => None
        }
        
        // 2. Lógica pura com base no resultado (usando toLongOption como fizeste na SaveLoadLogic)
        fileContentOpt match {
            case Some(content) =>
                content.toLongOption match {
                    case Some(seed) => MyRandom(seed)
                    case None       => MyRandom(7L) // Ficheiro tem lixo/texto inválido, recai no 7
                }
            case None =>
                MyRandom(7L) // Ficheiro não existe ou deu erro, recai no 7
        }
    }
    
    def saveRandomToFile(filename: String, rand: MyRandom): Unit = {
        try {
            val pw = new java.io.PrintWriter(new java.io.File(filename))
            pw.write(rand.seed.toString)
            pw.close()
        } catch {
            case e: Exception =>
                println(s"\n[Error] Could not save seed to file: ${e.getMessage}")
        }
    }
}