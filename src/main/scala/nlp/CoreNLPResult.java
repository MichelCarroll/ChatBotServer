package nlp;

import edu.stanford.nlp.trees.Tree;

import java.util.List;

public class CoreNLPResult {

    public Tree parseTree;
    public List<String> raws;
    public List<String> lemmas;
    public List<String> namedEntityTypes;
    public List<String> normalizedNamedEntities;

    public CoreNLPResult(Tree parseTree, List<String> raws, List<String> lemmas, List<String> namedEntityTypes, List<String> normalizedNamedEntities) {
        this.parseTree = parseTree;
        this.raws = raws;
        this.lemmas = lemmas;
        this.namedEntityTypes = namedEntityTypes;
        this.normalizedNamedEntities = normalizedNamedEntities;
    }
}
