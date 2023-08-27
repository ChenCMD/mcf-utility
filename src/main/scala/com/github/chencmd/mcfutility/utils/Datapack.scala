package com.github.chencmd.mcfutility.utils

import com.github.chencmd.mcfutility.utils.Path

import cats.Applicative
import cats.Show
import cats.effect.kernel.Async
import cats.implicits.*
import cats.mtl.Raise

import java.nio.file.FileSystems
import java.nio.file.Paths
import mouse.all.*

object Datapack {
  def getResourcePath(
    filePath: String,
    datapackRoot: String,
    fileType: Option[FileType] = None
  ): String = {
    val fileTypePath =
      fileType.orElse(getFileTypeOpt(filePath, datapackRoot)).map(_.folderName).getOrElse("[^/]+")
    s"^data/([^/]+)/$fileTypePath/(.*)\\.(?:mcfunction|json)".r
      .replaceAllIn(Path.relative(datapackRoot, filePath).replace("\\", "/"), "$1:$2")
  }

  def getDatapackRoot[F[_]](filePath: String)(using F: Async[F]): F[Option[String]] = {
    def isDatapackRoot(p: String): F[Boolean] = {
      FS.pathAccessible(Path.join(p, "pack.mcmeta"))
    }

    isDatapackRoot(filePath).ifM(
      F.pure(filePath.some),
      Path.dirname(filePath).flatTraverse(getDatapackRoot)
    )
  }

  def getNamespace(filePath: String, datapackRoot: String): String = {
    "^data/([^/]+)/.*$".r
      .replaceAllIn(Path.relative(datapackRoot, filePath).replace("\\", "/"), "$1")
  }

  enum IMPDocAccessor {
    case Private
    case Internal
    case Within(from: Map[Option[FileType], Set[String]])
    case Public
    case Api

    def toAccessorString: String = {
      this match {
        case Private      => "# @private"
        case Internal     => "# @internal"
        case Within(from) => {
            def toMessages(k: Option[FileType], v: List[String]): List[String] = {
              (v.size == 1).fold(
                List(s"${k.map(_.show + " ").orEmpty}${v.head}"),
                k.fold(v)(k => List(k.show) ::: v.map(" " * 4 + _))
              )
            }
            if (from.size == 1) {
              val messages = toMessages(from.head._1, from.head._2.toList.sorted)
              List(s"# @within ${messages.head}") ::: messages.tail.map("#" + _.drop(1))
            } else {
              val messages = for {
                from    <- from.toList
                message <- toMessages(from._1, from._2.toList.sorted)
              } yield s"#   $message"
              List("# @within") ::: messages
            }
          }.mkString("\n")
        case Public       => "# @public"
        case Api          => "# @api"
      }
    }
  }

  def generateIMPDoc[F[_]](
    resourcePath: String,
    accessor: IMPDocAccessor,
    comments: List[String] = List("")
  ): String = {
    s"""|#> $resourcePath
        |#
        |${comments.map(s => s.nonEmpty.fold(s"# $s", "#")).mkString("\n\n")}
        |#
        |${accessor.toAccessorString}
        |""".stripMargin
  }

  enum FileType(override val toString: String, val folderName: String, val pathPattern: String) {
    case Advancement extends FileType("advancement", "advancements", "data/*/advancements/**")
    case Dimension   extends FileType("dimension", "dimension", "data/*/dimension/**")
    case DimensionType
        extends FileType(
          "dimension_type",
          "dimension_type",
          "data/*/dimension_type/**"
        )
    case Function    extends FileType("function", "functions", "data/*/functions/**")
    case LootTable   extends FileType("loot_table", "loot_tables", "data/*/loot_tables/**")
    case Predicate   extends FileType("predicate", "predicates", "data/*/predicates/**")
    case Recipe      extends FileType("recipe", "recipes", "data/*/recipes/**")
    case Structure   extends FileType("structure", "structures", "data/*/structures/**/*.nbt")
    case TagBlock    extends FileType("tag/block", "tags/blocks", "data/*/tags/blocks/**")
    case TagEntityType
        extends FileType(
          "tag/entity_type",
          "tags/entity_types",
          "data/*/tags/entity_types/**"
        )
    case TagFluid    extends FileType("tag/fluid", "tags/fluids", "data/*/tags/fluids/**")
    case TagFunction extends FileType("tag/function", "tags/functions", "data/*/tags/functions/**")
    case TagItem     extends FileType("tag/item", "tags/items", "data/*/tags/items/**")
    case WorldgenBiome
        extends FileType(
          "worldgen/biome",
          "worldgen/biome",
          "data/*/worldgen/biome/**"
        )
    case WorldgenConfiguredCarver
        extends FileType(
          "worldgen/configured_carver",
          "worldgen/configured_carver",
          "data/*/worldgen/configured_carver/**"
        )
    case WorldgenConfiguredDecorator
        extends FileType(
          "worldgen/configured_decorator",
          "worldgen/configured_decorator",
          "data/*/worldgen/configured_decorator/**"
        )
    case WorldgenConfiguredFeature
        extends FileType(
          "worldgen/configured_feature",
          "worldgen/configured_feature",
          "data/*/worldgen/configured_feature/**"
        )
    case WorldgenConfiguredStructureFeature
        extends FileType(
          "worldgen/configured_structure_feature",
          "worldgen/configured_structure_feature",
          "data/*/worldgen/configured_structure_feature/**"
        )
    case WorldgenConfiguredSurfaceBuilder
        extends FileType(
          "worldgen/configured_surface_builder",
          "worldgen/configured_surface_builder",
          "data/*/worldgen/configured_surface_builder/**"
        )
    case WorldgenNoiseSettings
        extends FileType(
          "worldgen/noise_settings",
          "worldgen/noise_settings",
          "data/*/worldgen/noise_settings/**"
        )
    case WorldgenProcessorList
        extends FileType(
          "worldgen/processor_list",
          "worldgen/processor_list",
          "data/*/worldgen/processor_list/**"
        )
    case WorldgenTemplatePool
        extends FileType(
          "worldgen/template_pool",
          "worldgen/template_pool",
          "data/*/worldgen/template_pool/**"
        )
  }

  object FileType {
    given Show[FileType] = Show.fromToString[FileType]
  }

  val fs = FileSystems.getDefault()
  def getFileTypeOpt(filePath: String, datapackRoot: String): Option[FileType] = {
    val dir = s"${Path.relative(datapackRoot, filePath).replace("\\", "/")}/"
    FileType.values.find(f => fs.getPathMatcher(s"glob:${f.pathPattern}").matches(Paths.get(dir)))
  }

  def getFileType[F[_]](filePath: String, datapackRoot: String)(using
    F: Applicative[F],
    R: Raise[F, String]
  ): F[FileType] = {
    val dir = s"${Path.relative(datapackRoot, filePath).replace("\\", "/")}/"
    FileType.values
      .find(f => fs.getPathMatcher(s"glob:${f.pathPattern}").matches(Paths.get(dir)))
      .map(F.pure)
      .getOrElse(R.raise("The file type could not be determined"))
  }

  def parseTags(rawJson: String): List[String] = {
    ujson
      .read(rawJson)
      .obj("values")
      .arr
      .toList
      .map(v => v.strOpt.orElse(v.objOpt.map(_("id").str)).get)
  }
}
