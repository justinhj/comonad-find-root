package org.justinhj

import java.io.File
import org.apache.commons.io.FilenameUtils
import cats.implicits._

object ComonadFindRoot extends App {

  // Given a path use coflatmap to get all the parent path names and iterate over them
  // finding the first folder that has files matching a pattern
  def findRoot(path: File, rootFiles: List[String]): Option[File] = {
    val pathString = FilenameUtils.getPath(path.getCanonicalPath())
    val fullPath = FilenameUtils.getPath(pathString)
    val pathPrefix = if(FilenameUtils.getPrefix(pathString).length() == 0) 
                       File.separator
                     else 
                       FilenameUtils.getPrefix(pathString)
                       
    val candidatePaths = fullPath.
                          split(File.separatorChar).
                          toList.
                          reverse.
                          coflatMap(identity)

    def reversePathsToFile(paths: List[String], file: String = "") : File = {
      val path = pathPrefix + paths.reverse.intercalate(File.separator) + File.separator
      new File(path + file)
    }

    println(s"found ${candidatePaths.length} candidate paths\n$candidatePaths")
    val lazyPaths = LazyList(candidatePaths: _*)

    // TODO this is nice and all
    // but move it to Scala 3
    // tidy up the path stuff
    // needs to get the directory of files at each point and compare a regex
    lazyPaths.first(paths => {
      rootFiles.find(rf => reversePathsToFile(paths, rf).exists()).isDefined
    }).map(reversePaths => reversePathsToFile(reversePaths))
  }

  val cd = new File("/Users/justin.heyes-jones/projects/path-to-comonads/src/main/scala/org/justinhj/Pathtocomonads.scala")
  val found = findRoot(cd, List("build.sbt", "Cargo.toml"))
  found match {
    case Some(f) =>
      println(s"Root is $f")
    case None =>
      println("No root found")
  }
}
