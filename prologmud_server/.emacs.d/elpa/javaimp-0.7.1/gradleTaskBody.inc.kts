// use syntax which is valid both in Groovy and in Kotlin
doLast {
     println("id=${project.group};${project.path};${project.version}")
     if (project.parent != null) {
       println("parent-id=${project.parent.group};${project.parent.path};${project.parent.version}")
     }
     println("file=${project.buildFile}")
     println("build-dir=${project.buildDir}")
     if (project.pluginManager.hasPlugin("java")) {
       // "archives" configuration and "sourceSets" property defined by java plugin
       println("final-name=" + project.configurations.getByName("archives").artifacts.stream()
         .filter { it.type.equals("jar") || it.type.equals("war") }
         .map { it.file.path }
         .findFirst()
         .orElse(""))
       println("source-dirs=" + project.sourceSets.stream()
         .flatMap { it.allJava.srcDirs.stream().map { it.path } }
         .collect(Collectors.joining(File.pathSeparator)))
       println("dep-jars=" + project.sourceSets.stream()
         .flatMap { it.compileClasspath.files.stream().filter { it.name.endsWith("jar") }.map { it.path } }
         .collect(Collectors.joining(File.pathSeparator)))
     } else {
       println("final-name=")
       println("source-dirs=")
       println("dep-jars=")
     }
}
