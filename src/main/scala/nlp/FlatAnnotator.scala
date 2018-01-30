package nlp

import scala.collection.JavaConverters._

case class AnnotatedText(entities: List[Entity])

class FlatAnnotator(javaObject: CoreNLP) {

  def build(text: String): List[AnnotatedText] =
    javaObject.run(text).asScala.toList.map { result =>
      AnnotatedText(
        result.raws.asScala.toList
        .zip(result.lemmas.asScala.toList)
        .zip(result.namedEntityTypes.asScala.toList.map {
          case "O" => None
          case x => Some(x)
        })
        .zip(result.normalizedNamedEntities.asScala.toList.map {
          case null => None
          case x => Some(x)
        })
        .map { case (((x, y), z), w) =>
            Entity(
              x,
              y,
              z.map(label => NamedEntityType.fromLabel(label, w))
            )
        }
      )
    }

}
