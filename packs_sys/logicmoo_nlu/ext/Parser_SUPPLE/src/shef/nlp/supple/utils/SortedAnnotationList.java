package shef.nlp.supple.utils;



/* java stuff */
import gate.Annotation;
import gate.AnnotationSet;

import java.util.Iterator;
import java.util.Vector;


public class SortedAnnotationList extends Vector {

  public SortedAnnotationList() {
    super();
  }
  public boolean addSortedExclusive(Annotation annot) {
    Annotation currAnnot = null;
    for(int i=0; i<size() ; ++i) {
      currAnnot = (Annotation) get(i);
      if(annot.overlaps(currAnnot)) {
          return false;

      } //if

    } //for
    long annotStart = annot.getStartNode().getOffset().longValue();
    long currStart;
    for (int i=0; i < size(); ++i) {
      currAnnot = (Annotation) get(i);
      currStart = currAnnot.getStartNode().getOffset().longValue();
      if(annotStart < currStart) {
        insertElementAt(annot, i);
        return true;

      } //if

    } //for

    int size = size();
    insertElementAt(annot, size);
    return true;
  } //addSortedExclusive

  public boolean addSortedExclusiveAnnotation(AnnotationSet set)
  {
    boolean res=true;
    Iterator ite=set.iterator();
    Annotation ann;
    while(ite.hasNext()) {
      ann=(Annotation)ite.next();
      res=res && this.addSortedExclusive(ann);
    }
    return res;
  }
} //SortedAnnotationList