package sctags

import scala.tools.nsc.Global

import scala.collection._

trait TagGeneration { this: SCTags.type =>
  import compiler._
  def generateTags(tree: Tree): Seq[Tag] = {
    class GenTagTraverser extends compiler.Traverser {
      val _tags = new mutable.ListBuffer[Tag]
      var path = new mutable.Stack[String]
      def tags: Seq[Tag] = _tags.toList

      import compiler._

      def addTag(pos: TagPosition, name: String, fields: Map[String, String]) {
        if (!name.contains('$') && !name.equals("this")) {
          val fieldsList = "kind" -> fields("kind") :: fields.filterKeys(_ != "kind").toList
          _tags += Tag(name, pos, fieldsList: _*)
        }
      }

      def access(t: Tree) = t match {
        case (m: MemberDef) if m.mods.isPrivate   => Some("private")
        case (m: MemberDef) if m.mods.isProtected => Some("protected")
        case (m: MemberDef) if m.mods.isPublic    => Some("public")
        case _ => None
      }

      def implementation(t: Tree) = t match {
        case (m: MemberDef) if m.mods.isFinal         => Some("final")
        case (m: MemberDef) if m.mods.hasAbstractFlag => Some("abstract")
        case (m: MemberDef) if m.mods.isImplicit      => Some("implicit")
        case (m: MemberDef) if m.mods.isSealed        => Some("sealed")
        case (m: MemberDef) if m.mods.isLazy          => Some("lazy")
        case _ => None
      }

      def kind(t: Tree) = t match {
        case PackageDef(pid,_)                => Some("p")
        case ModuleDef(m,_,_)  if m.isCase  => Some("O")
        case ModuleDef(_,_,_)               => Some("o")
        case ClassDef(m,_,_,_) if m.isCase  => Some("C")
        case ClassDef(m,_,_,_) if m.isTrait => Some("t")
        case ClassDef(m,_,_,_)              => Some("c")
        case ValDef(m,_,_,_) if m.isMutable => Some("v")
        case ValDef(_,_,_,_)                => Some("V")
        case DefDef(_,name,_,_,_,_) if name != nme.CONSTRUCTOR => Some("m")
        case TypeDef(_,_,_,_)               => Some("T")
        case _ => None
      }

      def recurse(t: Tree) = t match {
        case DefDef(_,_,_,_,_,_) => false
        case TypeDef(_,_,_,_) => false
        case ValDef(_,_,_,_)  => false
        case _ => true
      }

      def signature(t: Tree) = t match {
        case DefDef(_,_,tparams,vparams,_,_)  =>
          val tsig =  tparams.map(_.name.decode) match {
            case List()  => ""
            case tl:List[String] => tl.mkString("[",", ","]")
          }
          val psig = vparams.foldLeft(""){
            (s,pl) =>
              s + pl.map{
                p => p.name.decode + ":" + p.tpt.toString
              }.mkString("(",", ",")")
          }
          Some(tsig + psig)

        case TypeDef(_,_,tparams,_) =>
          tparams.map(_.name.decode) match {
            case List()  => None
            case tl:List[string] => Some(tl.mkString("[",", ","]"))
          }
        case _ => None
      }

      override def traverse(t: Tree): Unit = {
        val line = t.pos.line
        val col  = t.pos.column
        val text = t.pos.lineContent
        var fields: immutable.Map[String, String] = immutable.Map.empty
        kind(t).foreach(fields += "kind" -> _)
        access(t).foreach(fields += "access" -> _)
        implementation(t).foreach(fields += "implementation" -> _)
        signature(t).foreach(fields += "signature" -> _)
        val name = t match {
          // TODO: handle package objects specially by matching PackageDef(ModuleDef) and ignoring
          // the module
          case pd: PackageDef => Some(pd.pid.toString)
          case dtree: DefTree => Some(dtree.name.toString)
          case _ => None
        }

        if (name.isDefined && fields.contains("kind")) {
          path.push(name.get)
          addTag(TagPosition(line, col, text), path.reverse.mkString("."), fields)
        }

        super.traverse(t)

        if (name.isDefined && fields.contains("kind")) {
          name.foreach(_ => path.pop())
        }
      }
    }
    val traverser = new GenTagTraverser
    traverser.traverse(tree)
    traverser.tags
  }
}
