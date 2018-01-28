package nlp;

import edu.stanford.nlp.trees.Tree;

import java.util.List;

public class CoreNLPResult {

    public Tree parseTree;
    public List<String> lemmas;

    public CoreNLPResult(Tree parseTree, List<String> lemmas) {
        this.parseTree = parseTree;
        this.lemmas = lemmas;
    }
}
