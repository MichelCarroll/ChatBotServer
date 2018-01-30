package nlp;

import edu.stanford.nlp.dcoref.CorefChain;
import edu.stanford.nlp.dcoref.CorefCoreAnnotations;
import edu.stanford.nlp.ling.CoreAnnotations;
import edu.stanford.nlp.ling.CoreLabel;
import edu.stanford.nlp.pipeline.Annotation;
import edu.stanford.nlp.pipeline.StanfordCoreNLP;
import edu.stanford.nlp.semgraph.SemanticGraph;
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations;
import edu.stanford.nlp.time.SUTime;
import edu.stanford.nlp.trees.Tree;
import edu.stanford.nlp.trees.TreeCoreAnnotations;
import edu.stanford.nlp.util.CoreMap;
import edu.stanford.nlp.time.TimeExpression;

import java.io.IOException;
import java.time.temporal.Temporal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class CoreNLP {

    StanfordCoreNLP pipeline;

    public CoreNLP() {
        Properties props = new Properties();
        props.put("annotators", "tokenize, ssplit, pos, lemma, parse, ner");
        this.pipeline = new StanfordCoreNLP(props);
    }

    public List<CoreNLPResult> run(String text) {

        // create an empty Annotation just with the given text
        Annotation document = new Annotation(text);

        // run all Annotators on this text
        this.pipeline.annotate(document);

        List<CoreNLPResult> results = new ArrayList<>();

        // these are all the sentences in this document
        // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
        List<CoreMap> sentences = document.get(CoreAnnotations.SentencesAnnotation.class);
        for(CoreMap sentence: sentences) {

            Tree tree = sentence.get(TreeCoreAnnotations.TreeAnnotation.class);

            List<String> raws = new ArrayList<>();
            List<String> lemmas = new ArrayList<>();
            List<String> namedEntityTypes = new ArrayList<>();
            List<String> normalizedNamedEntities = new ArrayList<>();

            for (CoreLabel token: sentence.get(CoreAnnotations.TokensAnnotation.class)) {
                raws.add(token.originalText());

                String lemma = token.get(CoreAnnotations.LemmaAnnotation.class);
                lemmas.add(lemma);

                String ne = token.get(CoreAnnotations.NamedEntityTagAnnotation.class);
                namedEntityTypes.add(ne);

                String norm = token.get(CoreAnnotations.NormalizedNamedEntityTagAnnotation.class);
                normalizedNamedEntities.add(norm);
            }

            results.add(new CoreNLPResult(tree, raws, lemmas, namedEntityTypes, normalizedNamedEntities));
        }

        return results;
    }

}
