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

    def restorePathName(path: List[String]): String =
      pathPrefix + path.reverse.intercalate(File.separator) + File.separator

    val candidatePaths = fullPath.
                          split(File.separatorChar).
                          toList.
                          reverse.
                          coflatMap(identity).map(restorePathName)
                          
    println(s"found ${candidatePaths.length} candidate paths\n$candidatePaths")
    val lazyPaths = LazyList(candidatePaths: _*)

    // TODO this is nice and all
    // but move it to Scala 3
    // needs to get the directory of files at each point and compare a regex
    lazyPaths.first(paths => {
      rootFiles.find(rf => new File(paths, rf).exists()).isDefined
    }).map(new File(_))
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
