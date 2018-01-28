package wordnet

import java.io.{File, FileInputStream, InputStream}

import net.didion.jwnl.JWNL
import net.didion.jwnl.data.list.{PointerTargetNode, PointerTargetNodeList}
import net.didion.jwnl.data.{IndexWord, PointerType, PointerUtils, Word}
import net.didion.jwnl.dictionary.Dictionary

import scala.collection.JavaConversions._
import scala.collection.breakOut
import scala.collection.mutable.ArrayBuffer

object WordNet {
  def apply(wnConfig: File): WordNet = new WordNet(new FileInputStream(wnConfig))
  def apply()              : WordNet = new WordNet(classOf[WordNet].getResourceAsStream("/wnconfig.xml"))
}
class WordNet(wnConfig: InputStream) {

  type Synset = net.didion.jwnl.data.Synset
  type POS    = net.didion.jwnl.data.POS

  JWNL.initialize(wnConfig)
  val dict            = Dictionary.getInstance()

  def allSynsets(pos: POS): Stream[Synset] =
    dict.getIndexWordIterator(pos)
      .flatMap(iword => iword.asInstanceOf[IndexWord].getSenses)
      .toStream

  def synsets(lemma: String): List[Synset] =
    net.didion.jwnl.data.POS.getAllPOS
      .flatMap(pos => synsets(lemma, pos.asInstanceOf[POS]))(breakOut)

  def synsets(lemma: String, pos: POS): List[Synset] = {
    val iword = dict.getIndexWord(pos, lemma)
    if (iword == null) List.empty[Synset]
    else iword.getSenses.toList
  }

  def getSynset(lemma: String, pos: POS, sid: Int): Option[Synset] = {
    val iword = dict.getIndexWord(pos, lemma)
    if (iword != null) Some(iword.getSense(sid))
    else None
  }

  def synset(lemma: String, pos: POS, sid: Int): Synset =
    getSynset(lemma, pos, sid)
      .getOrElse(throw new NoSuchElementException(s"lemma = '$lemma', pos = $pos, sid = $sid"))

  def synset(lemma: String, pos: POS): Synset = synset(lemma, pos, 1)

  def lemmas(s: String): List[Word] =
    synsets(s)
      .flatMap(ss => lemmas(ss))
      .filter(_.getLemma == s)

  def lemmas(ss: Synset): List[Word] = ss.getWords.toList

  def lemma(ss: Synset, wid: Int): Word = getLemma(ss, wid)
    .getOrElse(throw new NoSuchElementException(s"synset = $ss, wid = $wid"))

  def getLemma(ss: Synset, wid: Int): Option[Word] = {
    val l = lemmas(ss)
    if (l.size < wid) None else Some(l(wid))
  }

  def lemma(ss: Synset, lem: String): Word =
    getLemma(ss, lem).getOrElse(throw new NoSuchElementException(s"synset =  $ss, lem = $lem"))

  def getLemma(ss: Synset, lem: String): Option[Word] = {
    val words = ss.getWords
      .filter(w => lem == w.getLemma)
    words.headOption
  }

  def morphy(s: String, pos: POS): String = {
    val mp = dict.getMorphologicalProcessor
    val bf = mp.lookupBaseForm(pos, s)
    if (bf == null) "" else bf.getLemma
  }

  def morphy(s: String): String = {
    val m: List[String] = net.didion.jwnl.data.POS.getAllPOS.map(pos =>
      morphy(s, pos.asInstanceOf[POS]))(breakOut)
    val bases: List[String] = m.filterNot(_.isEmpty).distinct
    bases.headOption.getOrElse("")
  }

  ////////////////// Synset ///////////////////////////

  def lemmaNames(ss: Synset): List[String] =
    ss.getWords.map(_.getLemma)(breakOut)

  def definition(ss: Synset): String = {
    val g = ss.getGloss
    g.split(";")
      .filter(s => !isQuoted(s.trim()))
      .mkString(";")
  }

  def examples(ss: Synset): List[String] = {
    val g = ss.getGloss
    g.split(";")
      .collect {
        case s if isQuoted(s.trim()) => s.trim()
      } (breakOut)
  }

  def hyponyms          (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.HYPONYM           )
  def hypernyms         (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.HYPERNYM          )
  def partMeronyms      (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.PART_MERONYM      )
  def partHolonyms      (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.PART_HOLONYM      )
  def substanceMeronyms (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.SUBSTANCE_MERONYM )
  def substanceHolonyms (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.SUBSTANCE_HOLONYM )
  def memberHolonyms    (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.MEMBER_HOLONYM    )
  def entailments       (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.ENTAILMENT        )
  def entailedBy        (ss: Synset): List[Synset] = relatedSynsets(ss, PointerType.ENTAILED_BY       )

  def relatedSynsets(ss: Synset, ptr: PointerType): List[Synset] =
    ss.getPointers(ptr).map(ptr => ptr.getTarget.asInstanceOf[Synset])(breakOut)

  def hypernymPaths(ss: Synset): List[List[Synset]] =
    PointerUtils.getInstance()
      .getHypernymTree(ss)
      .toList
      .map(ptnl => ptnl.asInstanceOf[PointerTargetNodeList]
        .map(ptn => ptn.asInstanceOf[PointerTargetNode].getSynset)
        .toList)(breakOut)

  def rootHypernyms(ss: Synset): List[Synset] =
    hypernymPaths(ss)
      .map(hp => hp.reverse.head).distinct

  def lowestCommonHypernym(left: Synset, right: Synset): Option[Synset] = {
    val lPaths  = hypernymPaths(left)
    val rPaths  = hypernymPaths(right)
    lch(lPaths, rPaths)
  }

  private[this] def lch(lPaths: List[List[Synset]], rPaths: List[List[Synset]]): Option[Synset] = {
    val pairs   = for (lPath <- lPaths; rPath <- rPaths) yield (lPath, rPath)
    val lchs    = ArrayBuffer[(Synset,Int)]()
    pairs.map { case (lPath, rPath) =>
      val lSet    = lPath.toSet // Set(p1).flatten
    val matched = rPath.zipWithIndex.filter { case (s, i) => lSet.contains(s) }
      if (matched.nonEmpty) lchs += matched.head
    }
    if (lchs.isEmpty) None else Some(lchs.minBy(_._2)._1)
  }

  /** Note: this does not follow up on sister terms or hypoterms */
  def shortestHypernymPath(left: Synset, right: Synset): Option[(List[Synset], List[Synset])] = {
    val lPaths  = hypernymPaths(left )
    val rPaths  = hypernymPaths(right)
    lch(lPaths, rPaths).map { common =>
      val lPathsF   = lPaths.filter(_.contains(common))
      val rPathsF   = rPaths.filter(_.contains(common))
      val minLeft   = lPathsF.minBy(_.indexOf(common))
      val minRight  = rPathsF.minBy(_.indexOf(common))
      val l         = minLeft .take(minLeft .indexOf(common) + 1)
      val r         = minRight.take(minRight.indexOf(common) + 1)
      (l, r)
    }
  }

  def minDepth(ss: Synset): Int = {
    val lens = hypernymPaths(ss)
    if (lens.isEmpty) -1 else lens.map(_.size).min - 1
  }

  def format(ss: Synset): String =
    List(ss.getWord(0).getLemma,
      ss.getPOS.getKey,
      (ss.getWord(0).getIndex + 1).formatted("%02d"))
      .mkString(".")

  /////////////////// Words / Lemmas ////////////////////

  def antonyms(w: Word): List[Word] =
    relatedLemmas(w, PointerType.ANTONYM)

  def relatedLemmas(w: Word, ptr: PointerType): List[Word] =
    w.getPointers(ptr)
      .map(ptr => ptr.getTarget.asInstanceOf[Word])(breakOut)

  def format(w : Word): String =
    List(w.getSynset.getWord(0).getLemma,
      w.getPOS.getKey,
      (w.getIndex + 1).formatted("%02d"),
      w.getLemma)
      .mkString(".")

  ////////////////// misc ////////////////////////////////

  def isQuoted(s: String): Boolean =
    !s.isEmpty && s.charAt(0) == '"' && s.charAt(s.length() - 1) == '"'
}